%%
%% Copyright 2015 Joaquim Rocha <jrocha@gmailbox.org>
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(mec).

-behaviour(application).

-export([start/2, stop/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([open/2,
	call/3,
	call/4,
	call/5, 
	close/1]).

-spec open(Server :: Address, Port :: integer()) -> {ok, Pid :: pid()} | {error, Reason :: any()}
	when Address :: inet:ip_address() | inet:hostname().
open(Server, Port) when is_integer(Port) ->
	mec_sup:connection(Server, Port).

-spec call(Pid :: pid(), Operation :: binary(), Resource :: list()) ->
	{ok, Status :: integer(), Params :: list(), Payload :: empty | any()} | {error, Reason :: any()}.
call(Pid, Operation, Resource) ->
	call(Pid, Operation, Resource, [], empty).

-spec call(Pid :: pid(), Operation :: binary(), Resource :: list(), Params :: list()) ->
	{ok, Status :: integer(), Params :: list(), Payload :: empty | any()} | {error, Reason :: any()}.
call(Pid, Operation, Resource, Params) ->
	call(Pid, Operation, Resource, Params, empty).

-spec call(Pid :: pid(), Operation :: binary(), Resource :: list(), Params :: list(), Payload :: empty | any()) ->
	{ok, Status :: integer(), Params :: list(), Payload :: empty | any()} | {error, Reason :: any()}.
call(Pid, Operation, Resource, Params, Payload) ->
	gen_server:call(Pid, {call, Operation, Resource, Params, Payload}).

-spec close(Pid :: pid()) -> ok.
close(Pid) ->
	gen_server:cast(Pid, {shutdown}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start/2
start(_Type, _StartArgs) ->
	mec_sup:start_link().

%% stop/1
stop(_State) ->
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


