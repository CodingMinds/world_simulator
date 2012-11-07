%%%---------------------------------------------------------------------
%%% Description module world_sservsup
%%%---------------------------------------------------------------------
%%% The supervisor of the socket server which provides the user
%%% interface. The supervisor holds the listening socket. For each
%%% incoming connection request is a new child spawnd and monitored.
%%% Thanks to http://learnyousomeerlang.com/ for the code snippets.
%%%---------------------------------------------------------------------
%%% Exports
%%% start_link()
%%%   Interface for the behaviour supervisor.
%%%   Starts the supervisor.
%%% start_socket()
%%%   Starts a new child which can handle a new connection. Called from
%%%   the supervisor itself and other childs if they get activated.
%%% init([])
%%%   Interface for the behaviour supervisor.
%%%   Opens the listening socket and returns the child specification for
%%%   the supervisor.
%%%---------------------------------------------------------------------

-module(world_sservsup).
-author('M. Bittorf <info@coding-minds.com>').

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([start_socket/0]).

%%----------------------------------------------------------------------
%% Function: start_link/0
%% Purpose: Interface for the behaviour supervisor.
%%   Starts the supervisor.
%% Args: -
%% Returns: {ok, Pid} | ignore | {error, Reason}
%%----------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose: Interface for the behaviour supervisor.
%%   Opens the listening socket and creates a pool of childs which can
%%   be activated. After that returns the child specification for the
%%   supervisor.
%% Args: -
%% Returns: child_spec()
%%----------------------------------------------------------------------
init([]) ->
  {ok, Port} = application:get_env(port),
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active, once}, {packet, line}]),
  
  spawn_link(fun empty_listeners/0),
  
  {ok, {{simple_one_for_one, 60, 3600},
       [{socket,
        {world_sserv, start_link, [ListenSocket]},
        temporary, 400, worker, [world_sserv]}
       ]}}.

%%----------------------------------------------------------------------
%% Function: start_socket/0
%% Purpose: Starts a new child which can handle a new connection. Called
%% from the supervisor itself and other childs if they get activated.
%% Args: -
%% Returns: {ok, Child} | {ok, Child, Info} | {error, Reason}
%%----------------------------------------------------------------------
start_socket() ->
  supervisor:start_child(?MODULE, []).

%%----------------------------------------------------------------------
%% Function: empty_listeners/0
%% Purpose: Starts 20 listeners so that many multiple connections can be
%%   started at once, without serialization.
%% Args: -
%% Returns: ok
%%----------------------------------------------------------------------
empty_listeners() ->
  [start_socket() || _ <- lists:seq(1,20)],
  ok.
