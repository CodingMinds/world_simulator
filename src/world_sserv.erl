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
%%% handle_cast(world_changed, State)
%%%   Interface for the behaviour gen_server.
%%%   The world has changed.
%%% handle_cast(world_destroyed, State)
%%%   Interface for the behaviour gen_server.
%%%   The world was destroyed. Close the connection and Terminate.
%%% handle_cast(dead, State)
%%%   Interface for the behaviour gen_server.
%%%   The client is dead. Close the connection and Terminate.
%%% handle_info({tcp, Socket, "quit" ++ _}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'quit' messages.
%%% handle_info({tcp, Socket, "environ" ++ _}
%%%   Interface for the behaviour gen_server. 
%%%   Handle tcp 'environ' messages. Only applicable if an environment
%%%   is connected.
%%% handle_info({tcp, Socket, "move " ++ Message}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'move X' messages. Only applicable if an environment is
%%%   connected.
%%% handle_info({tcp, Socket, "help quit" ++ _}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'help quit' messages.
%%% handle_info({tcp, Socket, "help world" ++ _}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'help world' messages.
%%% handle_info({tcp, Socket, "help environ" ++ _}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'help environ' messages. Only applicable if an
%%%   environment is connected.
%%% handle_info({tcp, Socket, "help move" ++ _}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'help move' messages. Only applicable if an environment
%%%   is connected.
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
%% Returns: {ok, State}
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
%% Returns: {noreply, State} | {stop, {error, Reason}, State}
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
      world_helper:send(Socket, "200 Speak, friend, and ente(r)"),
      
      world_helper:log(info, "Socket ~w connection established",
        [Socket]),
      
      {noreply, State#sstate{socket=Socket}};
    {error, Reason} ->
      world_helper:log(error, "Socket connection error ~w", [Reason]),
      
      {stop, {error, Reason}, State}
  end;

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: The world has changed.
%% Args: -
%% Returns: {noreply, State}
%%----------------------------------------------------------------------
handle_cast(world_changed, State = #sstate{socket=Socket}) ->
  world_helper:send(Socket, "101 world changed"),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: The world was destroyed. Close the connection and terminate.
%% Args: -
%% Returns: {stop, normal, State}
%%----------------------------------------------------------------------
handle_cast(world_destroyed, State = #sstate{socket=Socket}) ->
  world_helper:send(Socket, "501 world destroyed"),
  close_connection(State),
  
  {stop, normal, State};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: The client is dead. Close the connection and terminate.
%% Args: -
%% Returns: {stop, normal, State}
%%----------------------------------------------------------------------
handle_cast(dead, State = #sstate{socket=Socket}) ->
  world_helper:send(Socket, "301 dead"),
  close_connection(State),
  
  {stop, normal, State}.

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'quit' message, cleanup the world and close
%%   socket.
%% Args: Socket package as tuple and server state as State
%% Returns: {stop, normal, State}.
%%----------------------------------------------------------------------
%% @doc Interface for the behaviour gen_server.
%%   Used to handle incoming tcp messages.
handle_info({tcp, _Socket, "quit" ++ _},
  State=#sstate{socket=Socket}) ->
  world_helper:log(info, "Socket ~w received quit", [Socket]),
  close_connection(State),
  
  {stop, normal, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'world load <Pid> [X Y]' messages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "world load " ++ String},
  State=#sstate{socket=Socket, environ=Env}) when is_atom(Env) ->
  Arguments = hd(string:tokens(String, "\r\n")),
  
  world_helper:log(info, "Socket ~w received world load ~s",
    [Socket, Arguments]),
  
  EnterWorld = fun(PidString, Xd, Yd) ->
    case world_helper:ascii_to_world(PidString) of
      false ->
        world_helper:send(Socket, "404 not found"),
        {noreply, State};
      Pid ->
        case gen_server:call(Pid, {birth, Xd, Yd}) of
          ok ->
            %% get map size
            {state, World} = gen_server:call(Pid, state),
            {X, Y} = world_helper:map_size(World#world.map),
            
            world_helper:send(Socket, "200 welcome in this ~Bx~B world."
              ++ " Your ID is ~w",
              [X, Y, self()]),
            
            {noreply, State#sstate{environ=Pid}};
          invalid_position ->
            world_helper:send(Socket, "405 invalid position"),
            
            {noreply, State};
          access_denied ->
            world_helper:send(Socket, "403 access denied"),
            
            {noreply, State};
          map_full ->
            world_helper:send(Socket, "403 access denied"),
            
            {noreply, State};
          Reason ->
            world_helper:send(Socket, "500 server made a boo boo"),
            close_connection(State),
            
            world_helper:log(error, "Socket ~w environ error ~w",
              [Socket, Reason]),
            
            {stop, {error, Reason}, State}
        end
    end
  end,
  
  case string:tokens(Arguments, " ") of
    [_] ->
      EnterWorld(Arguments, 0, 0);
    [P, X, Y] ->
      case catch {list_to_integer(X), list_to_integer(Y)} of
        {'EXIT', _} ->
          world_helper:send(Socket, "300 bad argument"),
          {noreply, State};
        {Xi, Yi} ->
          EnterWorld(P, Xi, Yi)
      end;
    _ ->
      world_helper:send(Socket, "300 bad argument"),
      {noreply, State}
  end;

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'world list' messages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "world list" ++ _},
  State=#sstate{socket=Socket, environ=Env}) when is_atom(Env) ->
  
  Worlds = [{Pid, gen_server:call(Pid, info)} || {_,Pid,_,_}
    <- supervisor:which_children(world_envsup)],
  AsciiWorlds = lists:map(
    fun({Pid, {info, Name, X, Y, Agents, MAgents}}) ->
      io_lib:format("~w ~s ~B ~B ~B ~B",
        [Pid, Name, X, Y, Agents, MAgents])
    end, Worlds),
  
  FormatedWorlds = "105 ID, Name, X, Y, Agents, possible Agents\r~n" ++
    "105 " ++ string:join(AsciiWorlds, "\r~n105 ") ++ "\r~n105 EOL",
  world_helper:send(Socket, FormatedWorlds),
  
  world_helper:log(info, "Socket ~w received world",
    [Socket]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'environ' message.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "environ" ++ _},
  State=#sstate{socket=Socket, environ=Env}) when is_pid(Env) ->
  call_world(State, environ),
  world_helper:log(info, "Socket ~w received environ", [Socket]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'move X' mesages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "move " ++ Message},
  State=#sstate{socket=Socket, environ=Env}) when is_pid(Env) ->
  case catch list_to_integer(hd(string:tokens(Message, "\r\n "))) of
    {'EXIT', _} ->
      world_helper:send(Socket, "300 bad argument");
    Direction ->
      call_world(State, {move, Direction})
  end,
  world_helper:log(info, "Socket ~w received move ~s",
    [Socket, Message]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'help quit' mesages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
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
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "help environ" ++ _},
  State=#sstate{socket=Socket, environ=Env}) when is_pid(Env) ->
  world_helper:send(Socket,
    "103 environ shows the nearest environ.\r~n" ++
    "103 Returns a 8 character long string with ASCII " ++
        "representations of the immediate neighbors.\r~n" ++
    "103 Possible characters are:\r~n" ++
    "103    .   Empty cell\r~n" ++
    "103    O   A blocking object\r~n" ++
    "103    F   Food\r~n" ++
    "103    *   Another agent\r~n" ++
    "103 The positions within the string corrspond with the mapping" ++
        "of Wilsons WOOD1 environment\r~n" ++
    "103    8 | 1 | 2\r~n" ++
    "103    7 | # | 3\r~n" ++
    "103    6 | 5 | 4"
  ),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'help move' mesages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "help move" ++ _},
  State=#sstate{socket=Socket, environ=Env}) when is_pid(Env) ->
  world_helper:send(Socket,
    "103 move [0-8] moves the client to position N.\r~n" ++
    "103 N must be a value from 0 to 8 and describes the direction " ++
        "relative to the actual position of the client. 0 means no " ++
        "move.\r~n" ++
    "103 The mapping is based on Wilsons WOOD1 environment\r~n" ++
    "103    8 | 1 | 2\r~n" ++
    "103    7 | 0 | 3\r~n" ++
    "103    6 | 5 | 4"
  ),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'help world' mesages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "help world" ++ _},
  State=#sstate{socket=Socket, environ=Env}) when is_atom(Env) ->
  world_helper:send(Socket,
    "103 world load ID   loads the world behing id ID.\r~n" ++
    "103 world list      shows all available worlds.\r~n" ++
    "103 The resulting list contains the ID, the internal name, the " ++
      "biggest X and Y values, the actual and the maximal amount of " ++
      "agents.\r~n" ++
    "103 A maximal amount of 0 should be interpreted als 'infinite'."
  ),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'help' mesages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "help" ++ _},
  State=#sstate{socket=Socket, environ=Env}) when is_pid(Env) ->
  world_helper:send(Socket,
    "103 The most commonly used commands are:\r~n" ++
    "103    help COMMAND      Print detailed help\r~n" ++
    "103    move [0-8]        Move the client to position N\r~n" ++
    "103    environ           Show the nearest environ\r~n" ++
    "103    quit              Leave this world"
  ),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'help' mesages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "help" ++ _},
  State=#sstate{socket=Socket}) ->
  world_helper:send(Socket,
    "103 The most commonly used commands are:\r~n" ++
    "103    help COMMAND      Print detailed help\r~n" ++
    "103    world list        List all available worlds\r~n" ++
    "103    world load ID     Load the world behind ID\r~n" ++
    "103    quit              Leave this world"
  ),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle empty tcp messages and ignore them
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "\r\n"}, State=#sstate{socket=Socket}) ->
  inet:setopts(Socket, [{active, once}]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle unknown tcp messages
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, Msg}, State=#sstate{socket=Socket}) ->
  world_helper:send(Socket, "400 unknown command"),
  world_helper:log(info, "Socket ~w received unknown command: ~s",
    [Socket, Msg]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle information about closed socket. Terminate the
%%   instance and remove mapping from environment.
%% Args: Socket state as tuple and server state as State
%% Returns: {stop normal, State}.
%%----------------------------------------------------------------------
handle_info({tcp_closed, _Socket}, State=#sstate{socket=Socket,
  environ=Env}) when is_pid(Env) ->
  
  case is_process_alive(Env) of
    true ->
      gen_server:call(Env, dead);
    _ ->
      ok
  end,
  
  world_helper:log(info, "Socket ~w closed", [Socket]),
  
  {stop, normal, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle information about closed socket. Terminate the
%%   instance.
%% Args: Socket state as tuple and server state as State
%% Returns: {stop normal, State}.
%%----------------------------------------------------------------------
handle_info({tcp_closed, _Socket}, State=#sstate{socket=Socket}) ->
  world_helper:log(info, "Socket ~w closed", [Socket]),
  
  {stop, normal, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle information abut socket error. Terminate the
%%   instance and remove mapping from environment.
%% Args: Socket state as tuple and server state as State
%% Returns: {stop, normal, State}.
%%----------------------------------------------------------------------
handle_info({tcp_error, _Socket, _}, State=#sstate{socket=Socket,
  environ=Env}) when is_pid(Env) ->
  
  case is_process_alive(Env) of
    true ->
      gen_server:call(Env, dead);
    _ ->
      ok
  end,
  
  world_helper:log(error, "Socket ~w closed", [Socket]),
  
  {stop, normal, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle information abut socket error. Terminate the
%%   instance.
%% Args: Socket state as tuple and server state as State
%% Returns: {stop, normal, State}.
%%----------------------------------------------------------------------
handle_info({tcp_error, _Socket, _}, State=#sstate{socket=Socket}) ->
  world_helper:log(error, "Socket ~w closed", [Socket]),
  
  {stop, normal, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle all other messages. Ignore them.
%% Args: Socket Package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info(E, S) ->
  world_helper:log(error, "unexpected: ~p", [E]),
  
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
%% Args: Actual server state State and command for the world
%%   Command.
%% Returns: ok | {error, Reason}.
%%----------------------------------------------------------------------
%% @doc Process command `Command' and send reply on socket `Socket'.
%% @private
call_world(#sstate{socket=Socket, environ=Env}, Command) ->
  case gen_server:call(Env, {do, Command}) of
    {ok, Environ} ->
      world_helper:send(Socket, "201 success ~s", [Environ]);
    {environ, Environ} ->
      world_helper:send(Socket, "102 environ ~s", [Environ]);
    {food, Environ} ->
      world_helper:send(Socket, "202 food ~s", [Environ]);
    {error, blocked, Environ} ->
      world_helper:send(Socket, "203 blocked ~s", [Environ]);
    {error, staffed, Environ} ->
      world_helper:send(Socket, "204 staffed ~s", [Environ]);
    {error, bad_arg} ->
      world_helper:send(Socket, "300 bad argument");
    {error, command_unknown} ->
      world_helper:send(Socket, "400 unknown command");
    {error, client_unknown} ->
      world_helper:send(Socket, "403 access denied");
    {error, dead} ->
      gen_server:cast(self(), dead),
      ok;
    {error, Reason} ->
      world_helper:send(Socket, "500 server made a boo boo"),      
      world_helper:log(error, "Socket ~w environ error ~w",
        [Socket, Reason])
  end.

%%----------------------------------------------------------------------
%% Function: close_connection/1
%% Purpose: Terminate connection and remove mapping from world.
%% Args: Actual server state State
%% Returns: ok
%%----------------------------------------------------------------------
%% @doc Terminate connection and remove mapping from world.
%% @private
close_connection(#sstate{socket=Socket, environ=Env})
  when is_pid(Env) ->
  world_helper:send(Socket, "200 good bye"),
  gen_server:call(Env, dead),
  gen_tcp:close(Socket),
  world_helper:log(info, "Socket ~w closed", [Socket]);

%%----------------------------------------------------------------------
%% Function: close_connection/1
%% Purpose: Terminate connection.
%% Args: Actual server state State
%% Returns: ok
%%----------------------------------------------------------------------
%% @doc Terminate connection.
%% @private
close_connection(#sstate{socket=Socket}) ->
  world_helper:send(Socket, "200 good bye"),
  gen_tcp:close(Socket),
  world_helper:log(info, "Socket ~w closed", [Socket]).
