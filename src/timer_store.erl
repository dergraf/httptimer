%% ------------------------------------------------------------------
%%
%% timer_store : Mnesia Access Module for HttpTimer
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
-module(timer_store).
-include("httptimer.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([insert/1, delete/1, lookup/1, all/0, retrieve_timers/1]).
-export([init/0]).

init() ->
	mnesia:create_table(httptimer, [{attributes, record_info(fields, httptimer)}, {disc_copies, [node()]}]).

insert(Timer) ->
	Transaction = fun() ->
		mnesia:write(Timer)
	end,
	case mnesia:transaction(Transaction) of
		{aborted, _Reason} -> false;
		{atomic, _ResultOfFun} ->true
	end.

delete(Key) ->
	Transaction = fun() ->
		mnesia:delete({httptimer, Key})
	end,
	case mnesia:transaction(Transaction) of
		{aborted, _Reason} -> false;
		{atomic, _ResultOfFun} ->true
	end.

lookup(Key) ->
	Transaction = fun() ->
		mnesia:read(httptimer, Key)
	end,
	case mnesia:transaction(Transaction) of
		{aborted, _Reason} -> false;
		{atomic, Data} -> Data
	end.

all() ->
	Map = fun(Record, Acc) ->
		Acc ++ [Record]
	end,

	Transaction = fun() ->
		mnesia:foldl(Map, [], httptimer)
	end,
	case mnesia:transaction(Transaction) of
		{aborted, _Reason} -> false;
		{atomic, Data} -> Data
	end.

retrieve_timers(Freshness) ->
	Map = fun(Record, Acc) ->
		Time = Record#httptimer.time,
		TimeMax = date_util:epoch() + Freshness,
		case (Time < TimeMax) and (Record#httptimer.status == inactive) of
			true -> Acc ++ [Record];
			false -> Acc
		end
	end,

	Transaction = fun() ->
		mnesia:foldl(Map, [], httptimer)
	end, 
	
	case mnesia:transaction(Transaction) of
		{aborted, Reason} -> Reason;
		{atomic, Data} -> Data
	end.
