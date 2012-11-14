%%%---------------------------------------------------------------------
%%% Description module world_envsup
%%%---------------------------------------------------------------------
%%% @author M. Bittorf <info@coding-minds.com>
%%% @copyright 2012 M. Bittorf
%%% @doc {@module} is the supervisor of the environments. This
%%% environments can be spawned and destroyed trough control interfaces.
%%% @reference Thanks to <a href="http://learnyousomeerlang.com/">Learn
%%% You Some Erlang</a> for the code snippets.
%%% @end
%%%---------------------------------------------------------------------
%%% Exports
%%% start_link()
%%%   Interface for the behaviour supervisor.
%%%   Starts the supervisor.
%%% spawn_world()
%%%   Creates a new world which can be accessed trough the interfaces.
%%% spawn_world(Arguments)
%%%   Creates a new world with the given Arguments which can be accessed
%%%   trough the interfaces.
%%% init([])
%%%   Interface for the behaviour supervisor.
%%%   Creates the first world and returns the child specification for
%%%   the supervisor.
%%%---------------------------------------------------------------------

-module(world_envsup).
-author('M. Bittorf <info@coding-minds.com>').

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([spawn_world/0, spawn_world/1]).

-include("world_records.hrl").

%%----------------------------------------------------------------------
%% Function: start_link/0
%% Purpose: Interface for the behaviour supervisor.
%%   Starts the supervisor.
%% Args: -
%% Returns: {ok, Pid} | ignore | {error, Reason}
%%----------------------------------------------------------------------
%% @doc Wrapper for start_link of supervisor.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose: Interface for the behaviour supervisor.
%%   Creates a new default world. After that returns the child
%%   specification for the supervisor.
%% Args: -
%% Returns: child_spec()
%%----------------------------------------------------------------------
%% @doc Interface for the behaviour supervisor.
%%   Creates a new default world. After that returns the child
%%   specification for the supervisor.
init([]) ->
  spawn_link(fun default_worlds/0),
  
  {ok, {{simple_one_for_one, 60, 3600},
       [{environment,
        {world_env, start_link, []},
        transient, 300, worker, [world_env]}
       ]}}.

%%----------------------------------------------------------------------
%% Function: spawn_world/0
%% Purpose: Creates a new world which can be accessed trough the
%%   interfaces.
%% Args: -
%% Returns: {ok, Child} | {ok, Child, Info} | {error, Reason}
%%----------------------------------------------------------------------
%% @docCreates a new world which can be accessed trough the interfaces.
spawn_world() ->
  spawn_world([]).

%%----------------------------------------------------------------------
%% Function: spawn_world/1
%% Purpose: Creates a new world which can be accessed trough the
%%   interfaces.
%% Args: A list of AdditionalArguments which shall passed to the world.
%% Returns: {ok, Child} | {ok, Child, Info} | {error, Reason}
%%----------------------------------------------------------------------
%% @docCreates a new world which can be accessed trough the interfaces.
spawn_world(AdditionalArguments) ->
  supervisor:start_child(?MODULE, [AdditionalArguments]).

%%----------------------------------------------------------------------
%% Function: default_worlds/0
%% Purpose: Spawns the initial default worlds.
%% Args: -
%% Returns: ok
%%----------------------------------------------------------------------
%% @doc Spawns the initial default worlds.
%% @private
default_worlds() ->
  
  %% Default 5x5 environment.
  Map1 = world_helper:ascii_to_map(".....|.....|OO.OO|...OF|....."),
  Options1 = #options{env_name="defaultMap", max_agents=4},
  
  %% Restrictive 9x9 environment
  Map2 = world_helper:ascii_to_map(".O.......|.O.OOOOO.|.O.OF....|"
    ".....OOOO|OOOO.....|FO.OOO.O.|.O.....O.|.O.OOOOO.|...O....."),
  Options2 = #options{env_name="restrictiveDemo", max_agents=2,
    allow_startposition=false},
  
  %% WOOD1 55x15
  WoodBlockRow =
    string:copies(".....", 11) ++ "|" ++
    string:copies(".OOF.", 11) ++ "|" ++
    string:copies(".OOO.", 11) ++ "|" ++
    string:copies(".OOO.", 11) ++ "|" ++
    string:copies(".....", 11) ++ "|",
  Wood1 = string:copies(WoodBlockRow, 3),
  
  Map3 = world_helper:ascii_to_map(Wood1),
  Options3 = #options{env_name="WOOD1", max_agents=1,
    allow_startposition=false},
  
  spawn_world([Map1, Options1]),
  spawn_world([Map2, Options2]),
  spawn_world([Map3, Options3]),
  
  ok.
