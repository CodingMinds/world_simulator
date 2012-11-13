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
  spawn_link(fun default_world/0),
  
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
%% Function: default_world/0
%% Purpose: Creates a simple default world.
%% Args: -
%% Returns: ok
%%----------------------------------------------------------------------
%% @doc Creates a simple default world.
%% @private
default_world() ->
  
  %% Add a default map for the first environment startup.
  Map = [
    {{1,1}, #sector{}},
    {{1,2}, #sector{}},
    {{1,3}, #sector{blocked=true}},
    {{1,4}, #sector{}},
    {{1,5}, #sector{staffed=true}},
    {{2,1}, #sector{}},
    {{2,2}, #sector{}},
    {{2,3}, #sector{blocked=true}},
    {{2,4}, #sector{}},
    {{2,5}, #sector{}},
    {{3,1}, #sector{}},
    {{3,2}, #sector{}},
    {{3,3}, #sector{}},
    {{3,4}, #sector{}},
    {{3,5}, #sector{}},
    {{4,1}, #sector{}},
    {{4,2}, #sector{}},
    {{4,3}, #sector{blocked=true}},
    {{4,4}, #sector{blocked=true}},
    {{4,5}, #sector{}},
    {{5,1}, #sector{}},
    {{5,2}, #sector{}},
    {{5,3}, #sector{blocked=true}},
    {{5,4}, #sector{food=75}},
    {{5,5}, #sector{}}
  ],
  
  spawn_world([Map]),
  ok.
