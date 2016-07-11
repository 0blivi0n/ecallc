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

-module(mec_conn).

-include("mec.hrl").

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
    {ok, Socket} ->
      Ref = monitor(process, Owner),
      {ok, #state{socket = Socket, owner = Ref}};
    {error, Reason} -> {stop, Reason}
  end.

%% handle_call/3
handle_call({call, Request, Timeout}, _From, State = #state{socket = Socket}) ->
  EncodedRequest = encode(Request),
  Reply = case gen_tcp:send(Socket, EncodedRequest) of
            ok ->
              case receive_reply(Socket, Timeout) of
                {ok, EncodedReply} ->
                  DecodedReply = decode(EncodedReply),
                  {ok, DecodedReply};
                Other -> Other
              end;
            Other -> Other
          end,
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  {noreply, State}.

%% handle_cast/2
handle_cast({shutdown}, State) ->
  {stop, shutdown, State};
handle_cast(_Request, State) ->
  {noreply, State}.

%% handle_info/2
handle_info({'DOWN', OwnerRef, _, _, _}, State = #state{owner = OwnerRef}) ->
  {stop, normal, State};
handle_info(_Info, State) ->
  {noreply, State}.

%% terminate/2
terminate(_Reason, #state{socket = Socket}) ->
  gen_tcp:close(Socket),
  ok.

%% code_change/3
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

encode(#mercury_request{operation = Operation, resource = Resource, params = Params, payload = Payload}) ->
  erlang:term_to_binary({request, Operation, Resource, Params, Payload}).

decode(EncodedReply) ->
  {reply, Status, Params, Payload} = erlang:binary_to_term(EncodedReply),
  #mercury_reply{status = Status, params = Params, payload = Payload}.

receive_reply(Socket, infinity) ->
  gen_tcp:recv(Socket, 0);
receive_reply(Socket, Timeout) ->
  gen_tcp:recv(Socket, 0, Timeout).