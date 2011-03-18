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
-include("httptimer.hrl").
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
	{ok, undefined}.

content_types_provided(ReqData, State) ->
	{[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, _Context) ->
	Response = {struct, [
			{method, wrq:method(ReqData)}, 
			{body, wrq:req_body(ReqData)},
			{headers, {struct, mochiweb_headers:to_list(wrq:req_headers(ReqData))}}
		]},
	JsonResponse = mochijson:encode(Response),
	io:format("JsonResponse : ~s~n", [JsonResponse]),
	{JsonResponse, ReqData, Response}.
