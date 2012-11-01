%%%---------------------------------------------------------------------
%%% Description module dummy
%%%---------------------------------------------------------------------
%%% Dummy holds some functions to simplifie the tests of all other
%%% modules (e.g. world).
%%%---------------------------------------------------------------------
%%% Exports
%%% world_start()
%%%   Starts the module world with a simple default world.
%%% world_status()
%%%   Prints the response of the gen_server:handle_call(status)
%%%---------------------------------------------------------------------

-module(dummy).
-author('M. Bittorf <info@coding-minds.com>').

-export([world_start/0, world_stop/0, world_state/0]).

-include("records.hrl").

%%----------------------------------------------------------------------
%% Function: world_start/0
%% Purpose: Calls gen_server:start_link/4 with a simple default map.
%% Args: -
%% Returns: {ok,Pid} | ignore | {error,Error}
%%----------------------------------------------------------------------
world_start() ->
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
  
  world:start(Map).

%%----------------------------------------------------------------------
%% Function: world_stop/0
%% Purpose: Calls gen_server:cast/2 with world, stop.
%% Args: -
%% Returns: {reply,Reply,NewState}
%%----------------------------------------------------------------------
world_stop() -> gen_server:cast(world, stop).

%%----------------------------------------------------------------------
%% Function: world_state/0
%% Purpose: Calls gen_server:call/2 with world, state.
%% Args: -
%% Returns: {reply,Reply,NewState}
%%----------------------------------------------------------------------
world_state() -> gen_server:call(world, state).
