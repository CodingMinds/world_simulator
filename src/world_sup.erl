%%%---------------------------------------------------------------------
%%% Description module world_sup
%%%---------------------------------------------------------------------
%%% @author M. Bittorf <info@coding-minds.com>
%%% @copyright 2012 M. Bittorf
%%% @doc The global supervisor {@module} which builds the supervision
%%% tree of the application world.
%%% @end
%%%---------------------------------------------------------------------
%%% Exports
%%% start_link()
%%%   Interface for the behaviour supervisor.
%%%   Starts the supervisor.
%%% init([])
%%%   Interface for the behaviour supervisor.
%%%   Returns the child specification for the supervisor.
%%%---------------------------------------------------------------------

-module(world_sup).
-author('M. Bittorf <info@coding-minds.com>').

-behaviour(supervisor).

-export([start_link/0, init/1]).

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
%%   Returns the child specification for the supervisor.
%% Args: -
%% Returns: child_spec()
%%----------------------------------------------------------------------
%% @doc Interface for the behaviour supervisor.
%%   Returns the child specification for the supervisor.
init([]) ->
  MaxRestart = 6,
  MaxTime = 3000,
  
  %% Add a default map for the world_env startup.
  Map = [
    {{1,1}, #sector{}},
    {{1,2}, #sector{}},
    {{1,3}, #sector{blocked=true}},
    {{1,4}, #sector{}},
    {{1,5}, #sector{staffed=1}},
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
  
  {ok,
   {{one_for_all, MaxRestart, MaxTime},
   [
    {ofo_supervisor,
     {world_ofo_sup, start_link, []},
     permanent, infinity, supervisor, [world_ofo_sup]},
    {env,
     {world_env, start_link, [Map]},
     permanent, 1500, worker, [world_env]}
   ]}}.
