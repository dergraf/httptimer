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
	{{trace, "/tmp"}, []}.	
	%%{ok, undefined}.

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
    {Res, NewState} = case wrq:path_info(apikey, ReqData) of
	undefined -> {false, State};
	ApiKey ->
	    case wrq:path_info(timerid, ReqData) of
		undefined -> {false, State};
		TimerId ->
		    case timer_manager:retrieve({ApiKey, TimerId}) of
			false -> {false, State};
			Timer -> {true, Timer}
		    end
	    end
    end,
    {Res, ReqData, NewState}.    

%% PUT Request
allowed_methods(ReqData, State) ->
	{['GET', 'HEAD', 'PUT', 'DELETE'], ReqData, State}.

content_types_accepted(ReqData, State) ->
	{[{"application/json", from_json}], ReqData, State}.

from_json(ReqData, State) ->
    {Res, NewState} = case wrq:path_info(apikey, ReqData) of
	undefined -> {false, State};
	ApiKey ->
	    case catch mochijson:decode(wrq:req_body(ReqData)) of
		{struct, Data} ->
		    case httptimer_helper:extract_httptimer(ApiKey, Data) of
			false -> {false, State};
			Timer ->
			    case timer_manager:add(Timer) of
				false -> {false, State};
				{_, TimerId} -> {true, {timerId, TimerId}}
			    end
		    end;
		{error,_} -> {false, State}
	    end
    end,
    {Res, append_json_response(ReqData,NewState), NewState}.

%% DELETE REQUEST
delete_resource(ReqData, State) ->
    {Res, NewState} = case wrq:path_info(apikey, ReqData) of
	undefined -> {false, State};
	ApiKey ->
	    case wrq:path_info(timerid, ReqData) of
		undefined -> {false, State};
		TimerId ->
		    case timer_manager:delete({ApiKey, TimerId}) of
			false -> {false, State};
			true -> {true, State}
		    end
	    end
    end,
    {Res, ReqData, NewState}.    

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
