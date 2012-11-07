%%%---------------------------------------------------------------------
%%% Description module world_ctl_sserv
%%%---------------------------------------------------------------------
%%% Ctl SServ is a socket based server which provides the interfaces to
%%% control the simulated world.
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
%%% handle_info({tcp, Socket, "map " ++ MapString}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'map X' messages.
%%% handle_info({tcp, Socket, "map" ++ _}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'map' messages.
%%% handle_info({tcp, Socket, "options " ++ OptionString}
%%%   Interface for the behaviour gen_server.
%%%   Handle tcp 'options ..' messages.
%%% handle_info({tcp, Socket, "options" ++ _}
%%%   Interface for the behaviour gen_server.
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
%% Purpose: Wrapper for start_link of gen_server
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
%% Purpose: Handle tcp 'quit' message. Terminate.
%% Args: Socket package as tuple and server state as State
%% Returns: {stop, normal, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "quit" ++ _},
  State=#sstate{socket=Socket}) ->
  world_helper:log(info, "Ctl: Socket ~w received quit", [Socket]),
  close_connection(Socket),
  
  {stop, normal, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'map MapString' mesages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "map " ++ String},
  State=#sstate{socket=Socket}) ->
  MapString = hd(string:tokens(String, "\r\n")),
  Map = world_helper:ascii_to_map(MapString),
  
  call_world(Socket, {map, Map}),
  world_helper:log(info, "Ctl: Socket ~w received map ~s",
    [Socket, [MapString]]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'map' message.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "map" ++ _},
  State=#sstate{socket=Socket}) ->
  call_world(Socket, map),
  world_helper:log(info, "Ctl: Socket ~w received map", [Socket]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'options OptionString' mesages.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, Socket, "options " ++ String},
  State=#sstate{socket=Socket}) ->
  case catch hd(string:tokens(String, "\r\n")) of
    {'EXIT', _} ->
      world_helper:send(Socket, "300 bad argument");
    OptionsString ->
      case world_helper:ascii_to_options(OptionsString) of
        {ok, Options} ->
          call_world(Socket, {options, Options});
        {error, _} ->
          world_helper:send(Socket, "300 bad argument")
      end
  end,
  world_helper:log(info, "Ctl: Socket ~w received new options ~s",
    [Socket, [String]]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle tcp 'options' message.
%% Args: Socket package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, "options" ++ _},
  State=#sstate{socket=Socket}) ->
  call_world(Socket, options),
  world_helper:log(info, "Ctl: Socket ~w received options", [Socket]),
  
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
  world_helper:log(info,
    "Ctl: Socket ~w received unknown command: ~s", [Socket, Msg]),
  
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle information about closed socket. Terminate.
%% Args: Socket state as tuple and server state as State
%% Returns: {stop normal, SState}.
%%----------------------------------------------------------------------
handle_info({tcp_closed, _Socket}, State=#sstate{socket=Socket}) ->
  gen_server:call(world_env, death),
  world_helper:log(info, "Ctl: Socket ~w closed", [Socket]),
  
  {stop, normal, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle information abut socket error. Terminate.
%% Args: Socket state as tuple and server state as State
%% Returns: {stop, normal, SState}.
%%----------------------------------------------------------------------
handle_info({tcp_error, _Socket, _}, State=#sstate{socket=Socket}) ->
  gen_server:call(world_env, death),
  world_helper:log(info, "Ctl: Socket ~w closed", [Socket]),
  
  {stop, normal, State};

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: Handle all other messages. Ignore them.
%% Args: Socket Package as tuple and server state as State
%% Returns: {noreply, SState}.
%%----------------------------------------------------------------------
handle_info(E, S) ->
  world_helper:log(info, "Ctl: unexpected: ~p", [E]),
  
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
%% Function: call_world/2
%% Purpose: Process command Command and send reply on socket Socket.
%% Args: Active connection socket Socket and command for the world
%%   Command.
%% Returns: ok | {error, Reason}.
%%----------------------------------------------------------------------
call_world(Socket, Command) ->
  case gen_server:call(world_env, Command) of
    ok ->
      world_helper:send(Socket, "201 success");
    {map, Map} ->
      AsciiRows = world_helper:map_to_ascii(Map),
      AsciiMap = "100 " ++ string:join(AsciiRows, "~n100 "),
      world_helper:send(Socket, AsciiMap);
    {options, Options} ->
      AsciiOptions = "104 " ++
        string:join(world_helper:options_to_ascii(Options), "~n104 "),
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
%% Purpose: Terminate connection and remove mapping from world
%% Args: Active socket Socket
%% Returns: ok
%%----------------------------------------------------------------------
close_connection(Socket) ->
  world_helper:send(Socket, "200 good bye"),
  gen_tcp:close(Socket),
  world_helper:log(info, "Ctl: Socket ~w closed", [Socket]).
