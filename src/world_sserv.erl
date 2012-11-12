%%%---------------------------------------------------------------------
%%% Description module world_sserv
%%%---------------------------------------------------------------------
%%% @author M. Bittorf <info@coding-minds.com>
%%% @copyright 2012 M. Bittorf
%%% @doc {@module} is a socket based server which provides the user
%%% interface for the simulated world. The socket server is based on
%%% the behaviour gen_server and handles tcp sessions which interpreted
%%% as a new client/agent.
%%% @end
%%%---------------------------------------------------------------------
%%% Exports
%%% init([])
%%%   Interface for the behaviour gen_server.
%%% handle_cast(accept, From, State)
%%%   Interface for the behaviour gen_server.
%%%   Informs the server to accept the incoming connection.
%%% handle_cast(world_changed, From, State)
%%%   Interface for the behaviour gen_server.
%%%   The world has changed.
%%% handle_cast(world_destroyed, From, State)
%%%   Interface for the behaviour gen_server.
%%%   The world was destroyed. Close the connection and Terminate.
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
%% Purpose: Wrapper for start_link of gen_server
%% Args: Socket
%% Returns: 
%%----------------------------------------------------------------------
%% @doc Wrapper for start_link of gen_server.
start_link(Socket) ->
  gen_server:start_link(?MODULE, Socket, []).

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose: Interface for the behaviour gen_server.
%%   Initialize a new socket server listening on socket Socket.
%% Args: The socket Socket on which we listen.
%% Returns: {ok, SState}
%%----------------------------------------------------------------------
%% @doc Interface for the behaviour gen_server.
%%   Initialize a new socket server listening on socket `Socket'.
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
%% @doc Interface for the behaviour gen_server.
%%   Used to initialize listening for incoming connections on our
%%   socket or handle global changes / announcements.
handle_cast(accept, State = #sstate{socket=LSocket}) ->
  %% handle new connection
  Accept = gen_tcp:accept(LSocket),
  
  %% request new acceptor from supervisor
  world_sservsup:start_socket(),
  
  %% evaluate accept response and try to create mapping
  case Accept of
    {ok, Socket} ->
      world_helper:log(info, "Socket ~w connection established",
        [Socket]),
      case gen_server:call(world_env, birth) of
        ok ->
          %% get map size
          {state, World} = gen_server:call(world_env, state),
          {X, Y} = world_helper:map_size(World#world.map),
          
          world_helper:send(Socket, "200 welcome in this ~Bx~B world",
            [X, Y]),
          
          {noreply, State#sstate{socket=Socket}};
        map_full ->
          world_helper:send(Socket, "403 access denied"),
          close_connection(Socket),
          
          {stop, {error, map_full}, State};
        _ ->
          world_helper:send(Socket, "500 server made a boo boo", []),
          close_connection(Socket),
          
          {stop, {error, unknown}, State}
      end;
    {error, Reason} ->
      world_helper:log(info, "Socket connection error ~w", [Reason]),
      
      {stop, {error, Reason}, State}
  end;

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: The world has changed.
%% Args: -
%% Returns: {noreply, SState}
%%----------------------------------------------------------------------
handle_cast(world_changed, SState = #sstate{socket=Socket}) ->
  world_helper:send(Socket, "101 world changed"),
  
  {noreply, SState};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: The world was destroyed. Close the connection and terminate.
%% Args: -
%% Returns: {stop, normal, SState}
%%----------------------------------------------------------------------
handle_cast(world_destroyed, SState = #sstate{socket=Socket}) ->
  world_helper:send(Socket, "501 world destroyed"),
  close_connection(Socket),
  
  {stop, normal, SState}.

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'quit' message, cleanup the world and close
%%   socket.
%% Args: Socket package as tuple and server state as State
%% Returns: {stop, normal, SState}.
%%----------------------------------------------------------------------
%% @doc Interface for the behaviour gen_server.
%%   Used to handle incoming tcp messages.
handle_info({tcp, _Socket, "quit" ++ _},
  State=#sstate{socket=Socket}) ->
  world_helper:log(info, "Socket ~w received quit", [Socket]),
  close_connection(Socket),
  
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
  world_helper:log(info, "Socket ~w received environ", [Socket]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'move X' mesages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "move " ++ Message},
  State=#sstate{socket=Socket}) ->
  case catch list_to_integer(hd(string:tokens(Message, "\r\n "))) of
    {'EXIT', _} ->
      world_helper:send(Socket, "300 bad argument");
    Direction ->
      call_world(Socket, {move, Direction})
  end,
  world_helper:log(info, "Socket ~w received move ~s",
    [Socket, Message]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'help quit' mesages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "help quit" ++ _},
  State=#sstate{socket=Socket}) ->
  world_helper:send(Socket,
    "103 quit leaves the world and closes the connection."),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'help environ' mesages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "help environ" ++ _},
  State=#sstate{socket=Socket}) ->
  world_helper:send(Socket,
    "103 environ shows the nearest environ.~n" ++
    "103 Returns a 8 character long string with ASCII " ++
        "representations of the immediate neighbors.~n" ++
    "103 Possible characters are:~n" ++
    "103    .   Empty cell~n" ++
    "103    O   A blocking object~n" ++
    "103    F   Food~n" ++
    "103    *   Another agent~n" ++
    "103 The positions within the string corrspond with the mapping" ++
        "of Wilsons WOOD1 environment~n" ++
    "103    8 | 1 | 2~n" ++
    "103    7 | # | 3~n" ++
    "103    6 | 5 | 4"
  ),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'help move' mesages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "help move" ++ _},
  State=#sstate{socket=Socket}) ->
  world_helper:send(Socket,
    "103 move [0-8] moves the client to position N.~n" ++
    "103 N must be a value from 0 to 8 and describes the direction " ++
        "relative to the actual position of the client. 0 means no " ++
        "move.~n" ++
    "103 The mapping is based on Wilsons WOOD1 environment~n" ++
    "103    8 | 1 | 2~n" ++
    "103    7 | 0 | 3~n" ++
    "103    6 | 5 | 4"
  ),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'help' mesages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "help" ++ _},
  State=#sstate{socket=Socket}) ->
  world_helper:send(Socket,
    "103 The most commonly used commands are:~n" ++
    "103    help COMMAND    Print detailed help~n" ++
    "103    move [0-8]      Move the client to position N~n" ++
    "103    environ         Show the nearest environ~n" ++
    "103    quit            Leave this world"
  ),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle empty tcp messages and ignore them
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "\r\n"}, State=#sstate{socket=Socket}) ->
  inet:setopts(Socket, [{active, once}]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle unknown tcp messages
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, Msg}, State=#sstate{socket=Socket}) ->
  world_helper:send(Socket, "400 unknown command"),
  world_helper:log(info, "Socket ~w received unknown command: ~s",
    [Socket, Msg]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle information about closed socket. Terminate.
%% Args: Socket state as tuple and server state as State
%% Returns: {stop normal, SState}.
%%----------------------------------------------------------------------
handle_info({tcp_closed, _Socket}, State=#sstate{socket=Socket}) ->
  gen_server:call(world_env, death),
  world_helper:log(info, "Socket ~w closed", [Socket]),
  
  {stop, normal, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle information abut socket error. Terminate.
%% Args: Socket state as tuple and server state as State
%% Returns: {stop, normal, SState}.
%%----------------------------------------------------------------------
handle_info({tcp_error, _Socket, _}, State=#sstate{socket=Socket}) ->
  gen_server:call(world_env, death),
  world_helper:log(info, "Socket ~w closed", [Socket]),
  
  {stop, normal, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle all other messages. Ignore them.
%% Args: Socket Package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info(E, S) ->
  world_helper:log(info, "unexpected: ~p", [E]),
  
  {noreply, S}.

%%----------------------------------------------------------------------
%% Function: *
%% Purpose: Dummy functions for the behaviour gen_server
%%----------------------------------------------------------------------
%% @doc Dummy function for the behaviour gen_server. Not used in this
%% implementation
handle_call(_Action, _From, State) ->{noreply, State}.
%% @doc Dummy function for the behaviour gen_server. Not used in this
%% implementation
terminate(normal, _State) -> ok;
terminate(_Reason, _State) -> ok.
%% @doc Dummy function for the behaviour gen_server. Not used in this
%% implementation
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%----------------------------------------------------------------------
%% Function: call_world/2
%% Purpose: Process command Command and send reply on socket Socket.
%% Args: Active connection socket Socket and command for the world
%%   Command.
%% Returns: ok | {error, Reason}.
%%----------------------------------------------------------------------
%% @doc Process command `Command' and send reply on socket `Socket'.
%% @private
call_world(Socket, Command) ->
  case gen_server:call(world_env, {do, Command}) of
    ok ->
      world_helper:send(Socket, "201 success");
    {environ, Environ} ->
      world_helper:send(Socket, "102 environ ~s", [Environ]);
    {food, Amount} ->
      world_helper:send(Socket, "202 food ~B", [Amount]);
    {error, blocked} ->
      world_helper:send(Socket, "203 blocked");
    {error, staffed} ->
      world_helper:send(Socket, "204 staffed");
    {error, bad_arg} ->
      world_helper:send(Socket, "300 bad argument");
    {error, command_unknown} ->
      world_helper:send(Socket, "400 unknown command");
    {error, client_unknown} ->
      world_helper:send(Socket, "403 access denied");
    {error, death} ->
      world_helper:send(Socket, "301 death");
    {error, _Reason} ->
      world_helper:send(Socket, "500 server made a boo boo")
  end.

%%----------------------------------------------------------------------
%% Function: close_connection/1
%% Purpose: Terminate connection and remove mapping from world
%% Args: Active socket Socket
%% Returns: ok
%%----------------------------------------------------------------------
%% @doc Terminate connection and remove mapping from world
%% @private
close_connection(Socket) ->
  world_helper:send(Socket, "200 good bye"),
  gen_tcp:close(Socket),
  gen_server:call(world_env, death),
  world_helper:log(info, "Socket ~w closed", [Socket]).
