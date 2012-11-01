%%%---------------------------------------------------------------------
%%% Description module sserver
%%%---------------------------------------------------------------------
%%% SServer is a socket based server which provides interfaces for the
%%% simulated world.
%%%---------------------------------------------------------------------
%%% Exports
%%% start(UserPort)
%%%   Starts the server.
%%%   The new servers listens on the port UserPort.
%%%---------------------------------------------------------------------

-module(sserver).
-author('M. Bittorf <info@coding-minds.com>').

-export([start/1]).

-include("records.hrl").

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

%%----------------------------------------------------------------------
%% Function: start/1
%% Purpose: Start the server and listen to the port UserPort
%% Args: UserPort as port where user commands will be accepted
%% Returns: -
%%----------------------------------------------------------------------
start(UserPort) ->
  dummy:world_start(),
  {ok, LSocket} = gen_tcp:listen(UserPort, ?TCP_OPTIONS),
  accept_user(LSocket).

%%----------------------------------------------------------------------
%% Function: accept_user/1
%% Purpose: Wait for incoming connections and spawn handler.
%% Args: Listener socket LSocket
%% Returns: -
%%----------------------------------------------------------------------
accept_user(LSocket) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	spawn(fun() ->
	  handle_connection(Socket)
	end),
  io:format("Socket ~w connection established~n", [Socket]),
	accept_user(LSocket).

%%----------------------------------------------------------------------
%% Function: handle_connection/1
%% Purpose: Handle incloming connection and create mapping in world.
%%   Also tell the client how big is our world.
%% Args: Active socket Socket
%% Returns: ok
%%----------------------------------------------------------------------
handle_connection(Socket) ->
  case gen_server:call(world, birth) of
    ok ->
      State = gen_server:call(world, state),
      Map = lists:map(fun({Coordinates, _}) -> Coordinates end, State#state.map),
      {X, Y} = lists:nth(1, lists:sort(fun({Xa, Ya}, {Xb, Yb}) -> (Xb < Xa) and (Yb < Ya) end, Map)),
      
      gen_tcp:send(Socket, list_to_binary(lists:append(["200 welcome in this ", integer_to_list(X), "x", integer_to_list(Y), " world\r\n"]))),
      loop_user(Socket);
    map_full ->
      gen_tcp:send(Socket, list_to_binary("403 access denied\r\n")),
      close_connection(Socket);
    _ ->
      gen_tcp:send(Socket, list_to_binary("500 server made a boo boo\r\n")),
      close_connection(Socket)
  end.

%%----------------------------------------------------------------------
%% Function: close_connection/1
%% Purpose: Terminate connection and remove mapping from world
%% Args: Active socket Socket
%% Returns: ok
%%----------------------------------------------------------------------
close_connection(Socket) ->
  gen_tcp:send(Socket, list_to_binary("200 good bye\r\n")),
  gen_tcp:close(Socket),
  gen_server:call(world, death),
  io:format("Socket ~w closed~n", [Socket]).

%%----------------------------------------------------------------------
%% Function: loop_user/1
%% Purpose: Handle active connection and communicate with the simulated
%%   world.
%% Args: Active connection socket Socket
%% Returns: ok.
%%----------------------------------------------------------------------
loop_user(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      String = string:strip(
                 string:strip(
                   string:strip(
                     string:strip(
                       binary_to_list(Data),
                     right, $\n),
                   right, $\r),
                 right, $\n),
               right, $\r),
      io:format("Socket ~w received ~p~n", [Socket, String]),
      
      case String of
        "quit" ->
          close_connection(Socket);
        "environ" ->
          call_world(Socket, environ),
          loop_user(Socket);
        "move 1" ->
          call_world(Socket, {move, 1}),
          loop_user(Socket);
        "move 2" ->
          call_world(Socket, {move, 2}),
          loop_user(Socket);
        "move 3" ->
          call_world(Socket, {move, 3}),
          loop_user(Socket);
        "move 4" ->
          call_world(Socket, {move, 4}),
          loop_user(Socket);
        "move 5" ->
          call_world(Socket, {move, 5}),
          loop_user(Socket);
        "move 6" ->
          call_world(Socket, {move, 6}),
          loop_user(Socket);
        "move 7" ->
          call_world(Socket, {move, 7}),
          loop_user(Socket);
        "move 8" ->
          call_world(Socket, {move, 8}),
          loop_user(Socket);
        _ ->
          io:format("Socket ~w unknown command~n", [Socket]),
          gen_tcp:send(Socket, list_to_binary("400 unknown command\r\n")),
          loop_user(Socket)
      end;
    {error, closed} ->
      gen_server:call(world, death),
      io:format("Socket ~w closed~n", [Socket]),
      ok
  end.
          %gen_tcp:send(Socket, list_to_binary("500 server made a boo boo\r\n")),
          %gen_tcp:send(Socket, list_to_binary("501 world destroyed\r\n")),

%%----------------------------------------------------------------------
%% Function: call_world/2
%% Purpose: Process command Command and send reply on socket Socket.
%% Args: Active connection socket Socket and command for the world
%%   Command.
%% Returns: ok | {error, Reason}.
%%----------------------------------------------------------------------
call_world(Socket, Command) ->
  io:format("Socket ~w ~w~n", [Socket, Command]),
  case gen_server:call(world, {do, Command}) of
    ok ->
      gen_tcp:send(Socket, list_to_binary("201 success\r\n"));
    {environ, Environ} ->
      gen_tcp:send(Socket, list_to_binary(lists:append(["102 environ ", Environ, "\r\n"])));
    {food, Amount} ->
      gen_tcp:send(Socket, list_to_binary(lists:append(["202 food ", integer_to_list(Amount), "\r\n"])));
    {error, blocked} ->
      gen_tcp:send(Socket, list_to_binary("203 blocked\r\n"));
    {error, staffed} ->
      gen_tcp:send(Socket, list_to_binary("204 staffed\r\n"));
    {error, bad_arg} ->
      gen_tcp:send(Socket, list_to_binary("300 bad argument\r\n"));
    {error, command_unknown} ->
      gen_tcp:send(Socket, list_to_binary("400 unknown command\r\n"));
    {error, client_unknown} ->
      gen_tcp:send(Socket, list_to_binary("403 access denied\r\n"));
    {error, death} ->
      gen_tcp:send(Socket, list_to_binary("301 death\r\n"));
    {error, _Reason} ->
      gen_tcp:send(Socket, list_to_binary("500 server made a boo boo\r\n"))
  end.


% admin return codes
%          gen_tcp:send(Socket, list_to_binary("100 map\r\n")),
%          gen_tcp:send(Socket, list_to_binary("101 world changed\r\n")),
%          
%          gen_tcp:send(Socket, list_to_binary("201 success\r\n")),
%          gen_tcp:send(Socket, list_to_binary("205 failed\r\n")),
%          
%          gen_tcp:send(Socket, list_to_binary("403 access denied\r\n")),
%          gen_tcp:send(Socket, list_to_binary("404 not found\r\n")),
%          
%          gen_tcp:send(Socket, list_to_binary("500 server made a boo boo\r\n")),
%          gen_tcp:send(Socket, list_to_binary("501 world destroyed\r\n")),
