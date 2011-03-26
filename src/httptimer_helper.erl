%% ------------------------------------------------------------------
%%
%% httptimer_helper : Helper module used to extract JSON values
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
%% --------------------------------------------------------------------

-module(httptimer_helper).
-include("httptimer.hrl").
-compile(export_all).

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


extract_httptimer(ApiKey, Data) ->
	Url = extract_string(Data, "url"),
	Method = extract_atom(Data, "method"),
	Time = extract_val(Data, "time"),
	Body = extract_string(Data, "body"),
	Headers = lists:map(fun(Tuple)->extract_key_val_pair(Tuple) end, extract_array(Data, "headers")),
	case 
		lists:member(Method, [get, put, post, delete]) andalso
		is_integer(Time) andalso Time > 0 of
		
		true ->
		        TimerId = erlang:md5(lists:flatten(io_lib:format("~p", [now()]))),
			#httptimer{
				id = {ApiKey,  hex:bin_to_hexstr(TimerId)},
				time = Time, %%date_util:epoch() + Time,
				created_on = date_util:epoch(),
				url = Url,
				headers = Headers,
				method = Method,
				body = Body
			};
		false -> false
	end.

