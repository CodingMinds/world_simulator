%%%---------------------------------------------------------------------
%%% Description module world_ctl_sserv
%%%---------------------------------------------------------------------
%%% @author M. Bittorf <info@coding-minds.com>
%%% @copyright 2012 M. Bittorf
%%% @doc {@module} is a socket based server which provides the
%%% interfaces to control the simulated world. The socket server is
%%% based on the behaviour gen_server and handles tcp sessions which can
%%% control the environment.
%%% @end
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
%%% handle_info({tcp, Socket, "world load" ++ String}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'world load <Pid>' messages.
%%% handle_info({tcp, Socket, "world spawn" ++ Map}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'world spawn Map' messages.
%%% handle_info({tcp, Socket, "world new" ++ _}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'world spawn' messages.
%%% handle_info({tcp, Socket, "world list" ++ _}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'world list' messages.
%%% handle_info({tcp, Socket, "world destroy" ++ Pid}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'world destroy <Pid>' messages.
%%% handle_info({tcp, Socket, "map " ++ MapString}
%%%   Interface for the behaviour gen_server. Only applicable if an
%%%   environment is connected.
%%%   Handle tcp 'map X' messages.
%%% handle_info({tcp, Socket, "map" ++ _}
%%%   Interface for the behaviour gen_server. Only applicable if an
%%%   environment is connected.
%%%   Handle tcp 'map' messages.
%%% handle_info({tcp, Socket, "options " ++ OptionString}
%%%   Interface for the behaviour gen_server. Only applicable if an
%%%   environment is connected.
%%%   Handle tcp 'options ..' messages.
%%% handle_info({tcp, Socket, "options" ++ _}
%%%   Interface for the behaviour gen_server. Only applicable if an
%%%   environment is connected.
%%%   Handle tcp 'options' messages.
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

-module(world_ctl_sserv).
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
%% Purpose: Wrapper for start_link of gen_server.
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
%%   Used to initialize listening for incoming connections on our socket
handle_cast(accept, State = #sstate{socket=LSocket}) ->
  %% handle new connection
  Accept = gen_tcp:accept(LSocket),
  
  %% request new acceptor from supervisor
  world_ctl_sservsup:start_socket(),
  
  %% evaluate accept response
  case Accept of
    {ok, Socket} ->
      world_helper:log(info, "Ctl: Socket ~w connection established",
        [Socket]),
      world_helper:send(Socket, "200 Speak, friend, and ente(r)"),
      
      {noreply, State#sstate{socket=Socket}};
    {error, Reason} ->
      world_helper:log(info, "Ctl: Socket connection error ~w",
        [Reason]),
      
      {stop, {error, Reason}, State}
  end.

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'quit' message. Terminates the instance.
%% Args: Socket package as tuple and server state as State
%% Returns: {stop, normal, State}.
%%----------------------------------------------------------------------
%% @doc Interface for the behaviour gen_server.
%%   Used to handle incoming tcp messages.
handle_info({tcp, _Socket, "quit" ++ _},
  State=#sstate{socket=Socket}) ->
  world_helper:log(info, "Ctl: Socket ~w received quit", [Socket]),
  close_connection(Socket),
  
  {stop, normal, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'world load <Pid>' messages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "world load " ++ String},
  State=#sstate{socket=Socket}) ->
  PidString = hd(string:tokens(String, "\r\n")),
  
  world_helper:log(info, "Ctl: Socket ~w received world load ~s",
    [Socket, PidString]),
  
  case world_helper:ascii_to_world(PidString) of
    false ->
      world_helper:send(Socket, "404 not found"),
      {noreply, State};
    Pid ->
      world_helper:send(Socket, "107 World ~w loaded", [Pid]),
      {noreply, State#sstate{environ=Pid}}
  end;

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'world spawn Map' messages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "world spawn " ++ String},
  State=#sstate{socket=Socket}) ->
  MapString = hd(string:tokens(String, "\r\n")),
  Map = world_helper:ascii_to_map(MapString),
  
  {ok, Pid} = world_envsup:spawn_world([Map]),
  
  world_helper:send(Socket, "106 World ~w spawned", [Pid]),
  
  world_helper:log(info, "Ctl: Socket ~w received world spawn ~s",
    [Socket, MapString]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'world spawn' messages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "world spawn" ++ _},
  State=#sstate{socket=Socket}) ->
  
  {ok, Pid} = world_envsup:spawn_world(),
  world_helper:send(Socket, "106 World ~w spawned", [Pid]),
  
  world_helper:log(info, "Ctl: Socket ~w received world spawn",
    [Socket]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'world destroy Pid' messages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "world destroy " ++ String},
  State=#sstate{socket=Socket}) ->
  PidString = hd(string:tokens(String, "\r\n")),
  
  world_helper:log(info, "Ctl: Socket ~w received world destroy ~s",
    [Socket, PidString]),
  
  
  case world_helper:ascii_to_world(PidString) of
    false ->
      world_helper:send(Socket, "404 not found"),
      {noreply, State};
    Pid ->
      world_helper:send(Socket, "108 World ~w destroyed", [Pid]),
      gen_server:cast(Pid, stop),
      {noreply, State#sstate{environ=undefined}}
  end;

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'world list' messages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "world list" ++ _},
  State=#sstate{socket=Socket}) ->
  
  Worlds = [{Pid, gen_server:call(Pid, info)} || {_,Pid,_,_}
    <- supervisor:which_children(world_envsup)],
  AsciiWorlds = lists:map(
    fun({Pid, {info, Name, X, Y, Agents, MAgents}}) ->
      io_lib:format("~w ~s ~B ~B ~B ~B",
        [Pid, Name, X, Y, Agents, MAgents])
    end, Worlds),
  
  FormatedWorlds = "105 ID, Name, X, Y, Agents, possible Agents~n" ++
    "105 " ++ string:join(AsciiWorlds, "~n105 ") ++ "~n105 EOL",
  world_helper:send(Socket, FormatedWorlds),
  
  world_helper:log(info, "Ctl: Socket ~w received world",
    [Socket]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'map MapString' messages. Only applicable if an
%%   environment is connected.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "map " ++ String},
  State=#sstate{socket=Socket, environ=Env}) when is_pid(Env) ->
  MapString = hd(string:tokens(String, "\r\n")),
  Map = world_helper:ascii_to_map(MapString),
  
  call_world(Socket, {map, Map}, Env),
  world_helper:log(info, "Ctl: Socket ~w received map ~s",
    [Socket, [MapString]]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'map' message. Only applicable if an environment
%%   is connected.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "map" ++ _},
  State=#sstate{socket=Socket, environ=Env}) when is_pid(Env) ->
  call_world(Socket, map, Env),
  world_helper:log(info, "Ctl: Socket ~w received map", [Socket]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'options OptionString' mesages. Only applicable
%%   if an environment is connected.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, Socket, "options " ++ String},
  State=#sstate{socket=Socket, environ=Env}) when is_pid(Env) ->
  case catch hd(string:tokens(String, "\r\n")) of
    {'EXIT', _} ->
      world_helper:send(Socket, "300 bad argument");
    OptString ->
      case length(string:tokens(OptString, " ")) of
        2 -> % a single option
          case catch gen_server:call(Env, options) of
            {'EXIT', {noproc, _}} ->
              world_helper:send(Socket, "404 not found");
            {'EXIT', _Reason} ->
              world_helper:send(Socket, "500 server made a boo boo");
            {options, Options} ->
              case world_helper:ascii_to_options(OptString, Options) of
                {ok, NewOptions} ->
                  call_world(Socket, {options, NewOptions}, Env);
                {error, _} ->
                  world_helper:send(Socket, "300 bad argument")
              end
          end;
        _ -> % all options
          case world_helper:ascii_to_options(OptString) of
            {ok, Options} ->
              call_world(Socket, {options, Options}, Env);
            {error, _} ->
              world_helper:send(Socket, "300 bad argument")
          end
      end
  end,
  world_helper:log(info, "Ctl: Socket ~w received new options ~s",
    [Socket, [String]]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'options' message. Only applicable if an
%%   environment is connected.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "options" ++ _},
  State=#sstate{socket=Socket, environ=Env}) when is_pid(Env) ->
  call_world(Socket, options, Env),
  world_helper:log(info, "Ctl: Socket ~w received options", [Socket]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'drop all' message. Only applicable if an
%%   environment is connected.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "drop all" ++ _},
  State=#sstate{socket=Socket, environ=Env}) when is_pid(Env) ->
  call_world(Socket, {drop, all}, Env),
  world_helper:log(info, "Ctl: Socket ~w received drop all", [Socket]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'drop dead' message. Only applicable if an
%%   environment is connected.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "drop dead" ++ _},
  State=#sstate{socket=Socket, environ=Env}) when is_pid(Env) ->
  call_world(Socket, {drop, dead}, Env),
  world_helper:log(info, "Ctl: Socket ~w received drop dead", [Socket]),
  
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
  world_helper:log(info,
    "Ctl: Socket ~w received unknown command: ~s", [Socket, Msg]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle information about closed socket. Terminate the
%%   instance.
%% Args: Socket state as tuple and server state as State
%% Returns: {stop normal, State}.
%%----------------------------------------------------------------------
handle_info({tcp_closed, _Socket}, State=#sstate{socket=Socket}) ->
  world_helper:log(info, "Ctl: Socket ~w closed", [Socket]),
  
  {stop, normal, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle information abut socket error. Terminate the
%%   instance.
%% Args: Socket state as tuple and server state as State
%% Returns: {stop, normal, State}.
%%----------------------------------------------------------------------
handle_info({tcp_error, _Socket, _}, State=#sstate{socket=Socket}) ->
  world_helper:log(info, "Ctl: Socket ~w closed", [Socket]),
  
  {stop, normal, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle all other messages. Ignore them.
%% Args: Socket Package as tuple and server state as State
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_info(E, S) ->
  world_helper:log(info, "Ctl: unexpected: ~p", [E]),
  
  {noreply, S}.

%%----------------------------------------------------------------------
%% Function: *
%% Purpose: Dummy functions for the behaviour gen_server. Not used in this
%% implementation
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
%% Function: call_world/3
%% Purpose: Process command Command in environment Env and send reply on
%%   socket Socket.
%% Args: Active connection socket Socket, command for the world
%%   Command and Env as environment pid.
%% Returns: ok | {error, Reason}.
%%----------------------------------------------------------------------
%% @doc Process command `Command' in environment `Env' and send reply on
%%   socket `Socket'.
%% @private
call_world(Socket, Command, Environment) ->
  case catch gen_server:call(Environment, Command) of
    {'EXIT', {noproc, _}} ->
      world_helper:send(Socket, "404 not found");
    {'EXIT', _Reason} ->
      world_helper:send(Socket, "500 server made a boo boo");
    ok ->
      world_helper:send(Socket, "201 success");
    {map, Map} ->
      AsciiRows = world_helper:map_to_ascii(Map),
      AsciiMap = "100 " ++ string:join(AsciiRows, "~n100 ") ++
        "~n100 EOL",
      world_helper:send(Socket, AsciiMap);
    {options, Options} ->
      AsciiOptions = "104 " ++
        string:join(world_helper:options_to_ascii(Options), "~n104 ") ++
          "~n104 EOL",
      world_helper:send(Socket, AsciiOptions);
    {error, bad_arg} ->
      world_helper:send(Socket, "300 bad argument");
    {error, command_unknown} ->
      world_helper:send(Socket, "400 unknown command");
    {error, _Reason} ->
      world_helper:send(Socket, "500 server made a boo boo")
  end.

%%----------------------------------------------------------------------
%% Function: close_connection/1
%% Purpose: Terminate connection.
%% Args: Active socket Socket.
%% Returns: ok
%%----------------------------------------------------------------------
%% @doc Terminate connection.
%% @private
close_connection(Socket) ->
  world_helper:send(Socket, "200 good bye"),
  gen_tcp:close(Socket),
  world_helper:log(info, "Ctl: Socket ~w closed", [Socket]).
