%%%---------------------------------------------------------------------
%%% Description module sserv
%%%---------------------------------------------------------------------
%%% SServ is a socket based server which provides interfaces for the
%%% simulated world.
%%%---------------------------------------------------------------------
%%% Exports
%%% init([])
%%%   Interface for the behaviour gen_server.
%%% handle_cast(accept, From, State)
%%%   Interface for the behaviour gen_server.
%%%   Informs the server to accept the incoming connection.
%%% handle_info({tcp, Socket, "quit" ++ _}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'quit' messages.
%%% handle_info({tcp, Socket, "environ" ++ _}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'environ' messages.
%%% handle_info({tcp, Socket, "move " ++ Message}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'move X' messages.
%%% handle_info({tcp, Socket, "help quit" ++ _}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'help quit' messages.
%%% handle_info({tcp, Socket, "help environ" ++ _}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'help environ' messages.
%%% handle_info({tcp, Socket, "help move" ++ _}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'help move' messages.
%%% handle_info({tcp, Socket, "help" ++ _}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'help' messages.
%%% handle_info({tcp, Socket, "\r\n"}
%%%   Interface for the behaviour gen_server.
%%%   Handle empty tcp messages.
%%% handle_info({tcp, Socket, _Msg}, State) ->
%%%   Interface for the behaviour gen_server.
%%%   Handle all unknown tcp messages.
%%% handle_info({tcp_closed, _Socket}, S) ->
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp_closed information. Terminate.
%%% handle_info({tcp_error, _Socket, _}, S) ->
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp_error information. Terminate.
%%% handle_info(E, S) ->
%%%   Interface for the behaviour gen_server.
%%%   Ignore all unknown messages.
%%%---------------------------------------------------------------------

-module(world_sserv).
-author('M. Bittorf <info@coding-minds.com>').

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-include("world_records.hrl").

-define(TCP_OPTIONS, [binary, {packet, line}, {active, once},
  {reuseaddr, true}]).

%%----------------------------------------------------------------------
%% Function: start/1
%% Purpose: Wraper for start_link of gen_server
%% Args: Socket
%% Returns: 
%%----------------------------------------------------------------------
start_link(Socket) ->
  gen_server:start_link(?MODULE, Socket, []).

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose: Interface for the behaviour gen_server.
%%   Initialice a new socket server listening on socket Socket.
%% Args: The socket Socket on which we listen.
%% Returns: {ok, SState}
%%----------------------------------------------------------------------
init(Socket) ->
  %% Because accepting a connection is a blocking function call,
  %% we can not do it in here. Forward to the server loop!
  gen_server:cast(self(), accept),
  
  {ok, #sstate{socket=Socket}}.

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Listen for incoming connections on our sstate socket and
%%   create a new acceptor from supervisor which handle the next
%%   connection.
%% Args: -
%% Returns: {noreply, SState} | {stop, {error, Reason}, SState}
%%----------------------------------------------------------------------
handle_cast(accept, State = #sstate{socket=LSocket}) ->
	case gen_tcp:accept(LSocket) of
	  {ok, Socket} ->
	    %% request new acceptor from supervisor
      world_sservsup:start_socket(),
      
      %% handle new connection and try to create mapping
      io:format("Socket ~w connection established~n", [Socket]),
      case gen_server:call(world_env, birth) of
        ok ->
          %% get map size
          World = gen_server:call(world_env, state),
          Map = lists:map(fun({Coordinates, _}) ->
                          Coordinates end, World#world.map),
          {X, Y} = lists:nth(1,
                     lists:sort(fun({Xa, Ya}, {Xb, Yb}) ->
                                (Xb < Xa) and (Yb < Ya) end, Map)),
          
          send(Socket, "200 welcome in this ~Bx~B world", [X, Y]),
          {noreply, State#sstate{socket=Socket}};
        map_full ->
          send(Socket, "403 access denied"),
          close_connection(Socket),
          {stop, {error, map_full}, State};
        _ ->
          send(Socket, "500 server made a boo boo", []),
          close_connection(Socket),
          {stop, {error, unknown}, State}
      end;
    {error, Reason} ->
      io:format("Socket connection error ~w~n", [Reason]),
      {stop, {error, Reason}, State}
  end.

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'quit' message, cleanup the world and close
%%   socket.
%% Args: Socket package as tuple and server state as State
%% Returns: {stop, normal, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "quit" ++ _},
  State=#sstate{socket=Socket}) ->
  close_connection(Socket),
  io:format("Socket ~w received quit~n", [Socket]),
  {stop, normal, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'environ' message.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "environ" ++ _},
  State=#sstate{socket=Socket}) ->
  call_world(Socket, environ),
  io:format("Socket ~w received environ~n", [Socket]),
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'move X' mesages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "move " ++ Message},
  State=#sstate{socket=Socket}) ->
  Direction = list_to_integer(hd(string:tokens(Message, "\r\n "))),
  call_world(Socket, {move, Direction}),
  io:format("Socket ~w received move ~B~n", [Socket, Direction]),
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'help quit' mesages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "help quit" ++ _},
  State=#sstate{socket=Socket}) ->
  send(Socket, "103 quit leaves the world and closes the connection."),
  io:format("Socket ~w received help quit~n", [Socket]),
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'help environ' mesages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "help environ" ++ _},
  State=#sstate{socket=Socket}) ->
  send(Socket, "103 environ shows the nearest environ.~n"
            ++ "103 Returns a 8 character long string with ASCII "
            ++ "representations of the immediate neighbors.~n"
            ++ "103 Possible characters are:~n"
            ++ "103    .   Empty cell~n"
            ++ "103    O   A blocking object~n"
            ++ "103    F   Food~n"
            ++ "103    *   Another agent~n"
            ++ "103 The positions within the string corrspond with the "
            ++ "mapping of Wilsons WOOD1 environment~n"
            ++ "103    8 | 1 | 2~n"
            ++ "103    7 | # | 3~n"
            ++ "103    6 | 5 | 4"
            ),
  io:format("Socket ~w received help environ~n", [Socket]),
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'help move' mesages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "help move" ++ _},
  State=#sstate{socket=Socket}) ->
  send(Socket, "103 move [1-8] moves the client to position N.~n"
            ++ "103 N must be a value from 1 to 8 and describes the "
            ++ "direction relative to the actual position of the "
            ++ "client.~n"
            ++ "103 The mapping is based on Wilsons WOOD1 environment~n"
            ++ "103    8 | 1 | 2~n"
            ++ "103    7 | # | 3~n"
            ++ "103    6 | 5 | 4"
            ),
  io:format("Socket ~w received help move~n", [Socket]),
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'help' mesages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "help" ++ _},
  State=#sstate{socket=Socket}) ->
  send(Socket, "103 The most commonly used commands are:~n"
            ++ "103    help COMMAND    Print detailed help~n"
            ++ "103    move [1-8]      Move the client to position N~n"
            ++ "103    environ         Show the nearest environ~n"
            ++ "103    quit            Leave this world"
            ),
  io:format("Socket ~w received help~n", [Socket]),
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle empty tcp messages and ignore them
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "\r\n"}, State=#sstate{socket=Socket}) ->
  inet:setopts(Socket, [{active, once}]),
  io:format("Socket ~w ignored empty line~n", [Socket]),
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle unknown tcp messages
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, Msg}, State=#sstate{socket=Socket}) ->
  send(Socket, "400 unknown command"),
  io:format("Socket ~w received unknown command: ~s~n", [Socket, Msg]),
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle information about closed socket. Terminate.
%% Args: Socket state as tuple and server state as State
%% Returns: {stop normal, SState}.
%%----------------------------------------------------------------------
handle_info({tcp_closed, _Socket}, State=#sstate{socket=Socket}) ->
  gen_server:call(world_env, death),
  io:format("Socket ~w closed~n", [Socket]),
  {stop, normal, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle information abut socket error. Terminate.
%% Args: Socket state as tuple and server state as State
%% Returns: {stop, normal, SState}.
%%----------------------------------------------------------------------
handle_info({tcp_error, _Socket, _}, State=#sstate{socket=Socket}) ->
  gen_server:call(world_env, death),
  io:format("Socket ~w closed~n", [Socket]),
  {stop, normal, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle all other messages. Ignore them.
%% Args: Socket Package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info(E, S) ->
  io:format("unexpected: ~p~n", [E]),
  {noreply, S}.

%%----------------------------------------------------------------------
%% Function: *
%% Purpose: Dummy functions for the behaviour gen_server
%%----------------------------------------------------------------------
handle_call(_Action, _From, State) ->{noreply, State}.
terminate(normal, _State) -> ok;
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%----------------------------------------------------------------------
%% Function: send/2
%% Purpose: Format message Str and send it to socket Socket
%% Args: Active socket Socket and message Str
%% Returns: ok
%%----------------------------------------------------------------------
send(Socket, Str) ->
  send(Socket, Str, []).

%%----------------------------------------------------------------------
%% Function: send/3
%% Purpose: Format message Str with arguments Args and send it to socket
%%   Socket
%% Args: Active socket Socket, format arguments Args and message Str
%% Returns: ok
%%----------------------------------------------------------------------
send(Socket, Str, Args) ->
  ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
  ok = inet:setopts(Socket, [{active, once}]),
  ok.

%%----------------------------------------------------------------------
%% Function: call_world/2
%% Purpose: Process command Command and send reply on socket Socket.
%% Args: Active connection socket Socket and command for the world
%%   Command.
%% Returns: ok | {error, Reason}.
%%----------------------------------------------------------------------
call_world(Socket, Command) ->
  case gen_server:call(world_env, {do, Command}) of
    ok ->
      send(Socket, "201 success");
    {environ, Environ} ->
      send(Socket, "102 environ ~s", [Environ]);
    {food, Amount} ->
      send(Socket, "202 food ~B", [Amount]);
    {error, blocked} ->
      send(Socket, "203 blocked");
    {error, staffed} ->
      send(Socket, "204 staffed");
    {error, bad_arg} ->
      send(Socket, "300 bad argument");
    {error, command_unknown} ->
      send(Socket, "400 unknown command");
    {error, client_unknown} ->
      send(Socket, "403 access denied");
    {error, death} ->
      send(Socket, "301 death");
    {error, _Reason} ->
      send(Socket, "500 server made a boo boo")
  end.

%%----------------------------------------------------------------------
%% Function: close_connection/1
%% Purpose: Terminate connection and remove mapping from world
%% Args: Active socket Socket
%% Returns: ok
%%----------------------------------------------------------------------
close_connection(Socket) ->
  send(Socket, "200 good bye"),
  gen_tcp:close(Socket),
  gen_server:call(world_env, death),
  io:format("Socket ~w closed~n", [Socket]).
