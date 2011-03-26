%% ------------------------------------------------------------------
%%
%% test_resource : Webmachine Test Resource
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

-module(test_resource).
-export([init/1, allowed_methods/2, content_types_provided/2, resource_exists/2, to_json/2, to_html/2, content_types_accepted/2, from_json/2, process_post/2, delete_resource/2]).
-include("httptimer.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
	{{trace, "/tmp"}, []}.
	%%{ok, undefined}.

create_record(ReqData) ->
	#httptimer{
		url = wrq:raw_path(ReqData),
		headers = mochiweb_headers:to_list(wrq:req_headers(ReqData)),
		method = wrq:method(ReqData),
		body = wrq:req_body(ReqData)
	}.

allowed_methods(ReqData, State) ->
	{['GET', 'POST', 'HEAD', 'PUT', 'DELETE'], ReqData, State}.

content_types_accepted(ReqData, State) ->
	{[{"application/json", from_json}], ReqData, State}.

from_json(ReqData, State) ->
	Timer = create_record(ReqData),
	ets:insert(test_timer_store, {wrq:path_info(query_string, ReqData), Timer}),
	{true, ReqData,State}.

process_post(ReqData, State) ->
	Timer = create_record(ReqData),
	ets:insert(test_timer_store, {wrq:path_info(query_string, ReqData), Timer}),
	{true, ReqData,State}.


content_types_provided(ReqData, State) ->
	{[{"application/json", to_json}, {"text/html", to_html}], ReqData, State}.

resource_exists(ReqData, State) ->
	case wrq:path_info(test, ReqData) of
		"timer" ->
			case ets:lookup(test_timer_store, wrq:path_info(query_string, ReqData)) of
				[Timer] -> {true, ReqData, Timer};
				[] -> {false, ReqData, State}
			end;
		_ ->
			{true, ReqData, store_timer_req}
	end.


to_json(ReqData, State) ->
	case State of
		store_timer_req ->
			Timer = create_record(ReqData),
			ets:insert(test_timer_store, {wrq:path_info(query_string, ReqData), Timer}),
			{mochijson:encode("test result stored"), ReqData,State};
		{_Key, Timer} ->
			Result = {struct, [
				{url, Timer#httptimer.url}, 
				{headers, {struct, Timer#httptimer.headers}}, 
				{method, Timer#httptimer.method}, 
				{body, Timer#httptimer.body}	
			]},
			{mochijson:encode(Result), ReqData, State}
	end.

delete_resource(ReqData, State) ->
	case wrq:path_info(test, ReqData) of
		"timer" ->
			ets:delete(test_timer_store, wrq:path_info(query_string, ReqData)),
			{true, ReqData, State};
		_ ->
			Timer = create_record(ReqData),
			ets:insert(test_timer_store, {wrq:path_info(query_string, ReqData), Timer}),
			{true, ReqData,State}
	end.

	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Provide Test Website
to_html(ReqData, Context) ->
	{ok, Value} = file:read_file("www/test.html"),
	{Value, ReqData, Context}.


