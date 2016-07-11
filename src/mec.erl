%%
%% Copyright 2015-16 Joaquim Rocha <jrocha@gmailbox.org>
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

-include("mec.hrl").

-export([start/2, stop/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([open/2,
         call/2,
         call/3,
         close/1]).

-spec open(Server :: Address, Port :: integer()) -> {ok, Pid :: pid()} | {error, Reason :: any()}
            when Address :: inet:ip_address() | inet:hostname().
open(Server, Port) when is_integer(Port) ->
  mec_sup:connection(Server, Port).

-spec call(Pid :: pid(), Request :: #mercury_request{}) ->
  {ok, Reply :: #mercury_reply{}} | {error, Reason :: any()}.
call(Pid, Request) ->
  call(Pid, Request, infinity).

-spec call(Pid :: pid(), Request :: #mercury_request{}, Timeout :: infinity | integer()) ->
  {ok, Reply :: #mercury_reply{}} | {error, Reason :: any()}.
call(Pid, Request, Timeout) ->
  gen_server:call(Pid, {call, Request, Timeout}).

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


