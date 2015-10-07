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

-module(mec_sup).

-define(SERVER, {local, ?MODULE}).

-behaviour(supervisor).

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,
	connection/2]).

start_link() ->
	supervisor:start_link(?SERVER, ?MODULE, []).

connection(Server, Port) ->
	supervisor:start_child(?MODULE, [Server, Port, self()]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
init([]) ->
	error_logger:info_msg("~p [~p] Starting...\n", [?MODULE, self()]),
	Connection = {mec_conn, {mec_conn, start, []}, temporary, 2000, worker, [mec_conn]},
	{ok,{{simple_one_for_one, 10, 60}, [Connection]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


