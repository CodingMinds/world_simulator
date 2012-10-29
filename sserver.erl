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
    gen_tcp:send(Socket, list_to_binary("200 welcome in this world\r\n")),
	  loop_user(Socket)
	end),
  io:format("Socket ~w connection established~n", [Socket]),
	accept_user(LSocket).

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
      String = string:strip(string:strip(binary_to_list(Data), right, $\n), right, $\r),
      io:format("Socket ~w received ~p~n", [Socket, String]),
      case String of
        "quit" ->
          gen_tcp:send(Socket, list_to_binary("200 good bye\r\n")),
          gen_tcp:close(Socket),
          gen_server:call(world, death),
          io:format("Socket ~w closed~n", [Socket]),
          ok;
        "TODO" ->
          io:format("Socket ~w TODO~n", [Socket]),
          gen_tcp:send(Socket, list_to_binary("500 TODO\r\n")),
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
