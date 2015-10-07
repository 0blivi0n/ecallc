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

-module(ecallc_connection).

-define(REQUEST(Operation, Resource, Params, Payload), {request, Operation, Resource, Params, Payload}).
-define(RESPONSE(Status, Payload), {response, Status, Payload}).
-define(SIMPLE_RESPONSE(Status), ?RESPONSE(Status, empty)).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/3]).

start_link(Server, Port, Owner) ->
	gen_server:start_link(?MODULE, [Server, Port, Owner], []).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {socket, owner}).

%% init/1
init([Server, Port, Owner]) ->
	case gen_tcp:connect(Server, Port, [{active, false}, binary, {keepalive, true}]) of
		{ok, Socket}  ->
			monitor(process, Owner),
    		{ok, #state{socket=Socket, owner=Owner}};
		{error, Reason} -> {stop,Reason}
	end.

%% handle_call/3
handle_call({call, Operation, Resource, Params, Payload}, _From, State=#state{socket=Socket}) ->
	Request = ?REQUEST(Operation, Resource, Params, Payload),
	EncodedRequest = erlang:term_to_binary(Request),
    Reply = case gen_tcp:send(Socket, EncodedRequest) of
		ok ->
			case gen_tcp:recv(Socket, 0) of
				{ok, Packet} ->
					Response = erlang:binary_to_term(Packet, [safe]),
					case Response of
						?SIMPLE_RESPONSE(Status) ->
							{ok, Status};
						?RESPONSE(Status, Data) ->
							{ok, Status, Data}
					end;
				Other -> Other
			end;
		Other -> Other
	end,
    {reply, Reply, State}.


%% handle_cast/2
handle_cast({shutdown}, State) ->
    {stop, shutdown, State}.


%% handle_info/2
handle_info({'DOWN', _MonitorRef, process, OwnerPid, shutdown}, State=#state{owner=OwnerPid}) ->
    {stop, normal, State}.


%% terminate/2
terminate(_Reason, #state{socket=Socket}) ->
	gen_tcp:close(Socket),
    ok.


%% code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


