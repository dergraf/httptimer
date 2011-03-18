%% ------------------------------------------------------------------
%%
%% httptimer : Startup code
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
-module(httptimer).
-include("httptimer.hrl").
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(crypto),
    ensure_started(inets),
    mnesia:create_schema([node()]),
    ensure_started(mnesia),
    ensure_started(mochiweb),
    ensure_started(ibrowse),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    httptimer_sup:start_link().

%% @spec start() -> ok
%% @doc Start the httptimer server.
start() ->
    ensure_started(crypto),
    ensure_started(inets),
    mnesia:create_schema([node()]),
    ensure_started(mnesia),
    ensure_started(mochiweb),
    ensure_started(ibrowse),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(httptimer),
    [timer_manager:add(Timer) || Timer <- timer_store:all()]. %% Load persisted Timers
    

%% @spec stop() -> ok
%% @doc Stop the httptimer server.
stop() ->
    Res = application:stop(httptimer),
    application:stop(webmachine),
    application:stop(ibrowse),
    application:stop(mochiweb),
    application:stop(mnesia),
    application:stop(inets),
    application:stop(crypto),
    Res.
