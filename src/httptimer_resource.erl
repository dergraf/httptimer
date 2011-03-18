%% ------------------------------------------------------------------
%%
%% httptimer_resource : Webmachine Resource for HttpTimer
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
%% Json API for accessing timer_manager
%% --------------------------------------------------------------------

-module(httptimer_resource).
-include("httptimer.hrl").
-export([init/1, content_types_provided/2, to_json/2, resource_exists/2]).
-export([allowed_methods/2, content_types_accepted/2, from_json/2, delete_resource/2]).
-export([is_authorized/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
	{ok, undefined}.



%% Helper Functions
extract_val(List, Key) ->
	case lists:keyfind(Key, 1, List) of
		false -> undefined;
		{_, Val} -> Val
	end.

extract_string(List, Key) ->
	case extract_val(List, Key) of
		undefined -> "";
		Val -> 
			case is_list(Val) of
				false -> "";
				true -> Val
			end
	end.

extract_array(List, Key) ->
	case extract_val(List, Key) of
		undefined -> [];
		{array, Val} -> Val
	end.

extract_key_val_pair({struct,[{Key,Value}]}) -> {Key, Value}.

extract_atom(List, Key) ->
	case extract_string(List, Key) of
		"" -> undefined;
		Val -> list_to_atom(Val)
	end.

append_json_response(ReqData, Data) ->
	wrq:append_to_response_body(
		mochijson:encode({struct, [Data]}), ReqData).



content_types_provided(ReqData, State) ->
	{[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, Timer) ->
	Result = {struct, [
		{url, Timer#httptimer.url}, 
		{time, Timer#httptimer.time}, 
		{created_on, Timer#httptimer.created_on}, 
		{headers, {struct, Timer#httptimer.headers}}, 
		{method, Timer#httptimer.method}, 
		{body, Timer#httptimer.body}
	]},
	{mochijson:encode(Result), ReqData, Result}.


%% GET Request
resource_exists(ReqData, State) ->
	case catch mochijson:decode(wrq:req_body(ReqData)) of
		{struct, Data} -> process_get_request(ReqData, State, Data);
		_ ->{false, append_json_response(ReqData,{error, "Please check your JSON Request"}), State}
	end.
	
process_get_request(ReqData, State, Data) ->
	Key = extract_val(Data, "key"),
	TimerId = extract_val(Data, "timerId"),

	case
		erlang:is_integer(Key) andalso 
		erlang:is_integer(TimerId) of
		
		true ->
			case timer_manager:retrieve({Key, TimerId}) of
				false -> {false, ReqData, State};
				Timer -> {true, ReqData, Timer}
			end;
		false ->
			{false, ReqData, State}
	end.
							

%% PUT Request
allowed_methods(ReqData, State) ->
	{['GET', 'HEAD', 'PUT', 'DELETE'], ReqData, State}.

content_types_accepted(ReqData, State) ->
	{[{"application/json", from_json}], ReqData, State}.

from_json(ReqData, State) ->
	case catch mochijson:decode(wrq:req_body(ReqData)) of
		{struct, Data} -> process_put_request(ReqData, State, Data);
		{error,_} ->{false, append_json_response(ReqData,{error, "Please check your Request"}), State}
 	end.

process_put_request(ReqData, State, Data) ->
	Key = extract_val(Data, "key"),
	Url = extract_string(Data, "url"),
	Method = extract_atom(Data, "method"),
	Time = extract_val(Data, "time"),
	Body = extract_string(Data, "body"),
	Headers = lists:map(fun(Tuple)->extract_key_val_pair(Tuple) end, extract_array(Data, "headers")),
	case 
		erlang:is_integer(Key) andalso 
		lists:member(Method, [get, put, post, delete]) andalso
		erlang:is_integer(Time) andalso Time > 0 of
		
		true ->
			Timer = #httptimer{
				id = {Key,  erlang:phash2(now())},
				time = Time, %%date_util:epoch() + Time,
				created_on = date_util:epoch(),
				url = Url,
				headers = Headers,
				method = Method,
				body = Body
			},
			case timer_manager:add(Timer) of
				false ->
					{false, append_json_response(ReqData,{error, "Cannot add Timer"}), State};
				{_, TimerId} ->
					{true, append_json_response(ReqData,{timerId, TimerId}), State}
			end;	
		false ->
			{false, append_json_response(ReqData,{error, "Please check your Request"}), State}
	end.

delete_resource(ReqData, State) ->
	case catch mochijson:decode(wrq:req_body(ReqData)) of
		{struct, Data} -> process_delete_request(ReqData, State, Data);
		{error,_} -> {false, append_json_response(ReqData,{error, "Please check your Request"}), State}
	end.

process_delete_request(ReqData, State, Data) ->
	Key = extract_val(Data, "key"),
	TimerId = extract_val(Data, "timerId"),

	case
		erlang:is_integer(Key) andalso 
		erlang:is_integer(TimerId) of
		
		true -> 
			case timer_manager:delete({Key, TimerId}) of
				false -> 
					{false, append_json_response(ReqData, {error, "Cannot cancel Timer"}), State};
				true ->
					{true, ReqData, State}
			end;
		false ->
			{false, append_json_response(ReqData, {error, "Please check your Request"}), State}
	end.

%% Support for Authorization
-define(AUTH_HEAD, "Basic realm=HttpTimer").

is_authorized(ReqData, State) ->
	case wrq:method(ReqData) of
		PD when PD == 'PUT'; PD == 'DELETE'; PD == 'GET' -> basic_auth(ReqData, State);
		_ -> {?AUTH_HEAD, ReqData, State}
	end.

basic_auth(ReqData, State) ->
	case wrq:get_req_header("Authorization", ReqData) of
		"Basic" ++ Base64 ->
			{ok, AcceptedUser} = application:get_env(httptimer, user),
			{ok, AcceptedPassword} = application:get_env(httptimer, password),
			case string:tokens(base64:mime_decode_to_string(Base64), ":") of
				[AcceptedUser, AcceptedPassword] -> {true, ReqData, State};
				_ -> {?AUTH_HEAD, ReqData, State}
			end;
		_ -> {?AUTH_HEAD, ReqData, State}
	end.
