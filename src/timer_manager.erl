%% ------------------------------------------------------------------
%%
%% timer_manager : Timer Backend of HttpTimer
%%
%% Copyright (c) 2011 Andre Graf <andre@graf.io>, All Rights Reserved
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
%% Adding, retrieving, deleting timers from/to the timer_store (mnesia)
%% Besides these controlling functions this modules schedules and 
%% executes the timers.
%% --------------------------------------------------------------------
-module(timer_manager).
-behaviour(gen_server).
-include("httptimer.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, retrieve/1, add/1, delete/1, execute/1]).

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	timer_store:init(),
	{ok,TimerFreshness} = application:get_env(httptimer, timer_freshness),
	{ok,RetrievalPeriod} = application:get_env(httptimer, retrieval_period),
	spawn(fun() -> retrieve_next_active_timers({RetrievalPeriod, TimerFreshness}) end),
	{ok, []}.

retrieve(TimerId) ->
	case timer_store:lookup(TimerId) of
		[Timer] -> Timer;
		[] -> false;
		badarg -> false
	end.


add(Timer) ->
	{ok,RetrievalPeriod} = application:get_env(httptimer, retrieval_period),
	case timer_store:insert(Timer#httptimer{status=inactive}) of
		true -> %% successfully insert timer
			case (Timer#httptimer.time - date_util:epoch()) < RetrievalPeriod of
				true -> %% immediate schedule (delayed)
					schedule_timer(Timer),
					Timer#httptimer.id;
				false -> %% normal, just return the TimerId 
					Timer#httptimer.id
			end;
		false -> %% error during inserting timer
			false
	end.

delete(TimerId) ->
	 case timer_store:lookup(TimerId) of
		[#httptimer{tref=TRef, status=scheduled}]->
			case timer:cancel(TRef) of
				{ok, cancel} -> 
					timer_store:delete(TimerId),
					error_logger:info_msg("Timer canceled ~w~n", [TRef]),
					true;
				{error, Reason} ->
					error_logger:info_msg("Cannot cancel Timer ~w Error ~w~n", [TRef, Reason]),
					false
			end;
		[#httptimer{tref=TRef, status=inactive}]->
			timer_store:delete(TimerId),
			error_logger:info_msg("Unscheduled Timer deleted~n", []),
			true;
		[] ->
			error_logger:info_msg("Cannot find a Timer for given Id ~w~n", [TimerId]),
			false;
		badarg ->
			false	
	end.

execute(Timer) ->
	error_logger:info_msg("Execute Timer with Id ~w", [Timer#httptimer.id]),
	case ibrowse:send_req(
			Timer#httptimer.url, 
			Timer#httptimer.headers,
			Timer#httptimer.method,
			Timer#httptimer.body) of
		{ok, Status, _, _} ->
			timer_store:delete(Timer#httptimer.id),
			error_logger:info_msg("Url: ~s Status: ~s~n", [Timer#httptimer.url, Status]),
			true;
		{error, Reason} ->
			UpdatedTimer = Timer#httptimer{status=execution_failed},
			timer_store:insert(UpdatedTimer),
			error_logger:info_msg("Url: ~s Error: ~s~n", [Timer#httptimer.url, Reason]),
			false
	end.

schedule_timer(Timer) ->
	gen_server:cast(?MODULE, {schedule_timer, Timer}).

retrieve_next_active_timers({RetrievalPeriod, TimerFreshness}) ->
	receive
		%% Changing Period
		{NewRetrievalPeriod, NewTimerFreshness} when 
			is_number(NewRetrievalPeriod) and is_number(NewTimerFreshness) -> 
				retrieve_next_active_timers({NewRetrievalPeriod, NewTimerFreshness})
	after RetrievalPeriod ->
		error_logger:info_msg("retrieve timers for scheduling~n"),
		[schedule_timer(Timer) || Timer <- timer_store:retrieve_timers(TimerFreshness)],
		retrieve_next_active_timers({RetrievalPeriod, TimerFreshness})
	end.


handle_call(_Message, _FROM, State) -> {reply, false, State}.

handle_cast({schedule_timer, Timer}, State) -> 
	Now = date_util:epoch(),
	Time = case (Timer#httptimer.time - Now) < 0 of 
		true -> 0; %% can happen in case of expired timer entries
		false -> Timer#httptimer.time - Now
	end,
	case timer:apply_after(Time * 1000, ?MODULE, execute, [Timer]) of
		{error, Reason} ->
			error_logger:info_msg("Cannot schedule Timer ~w~n", [Reason]);
		{ok, TRef} ->
			UpdatedTimer = Timer#httptimer{tref=TRef, status=scheduled},
			timer_store:insert(UpdatedTimer),
			error_logger:info_msg("Scheduled Timer ~w is executed in ~ws ~n", [Timer#httptimer.id, Time])
	end,
	{noreply, State}.


% avoid compile warnings
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
