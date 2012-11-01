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
    {{0,0}, #sector{food=1, blocked=false}},
    {{0,1}, #sector{food=0, blocked=true}},
    {{0,2}, #sector{food=0, blocked=false}},
    {{1,0}, #sector{food=1, blocked=false}},
    {{1,1}, #sector{food=0, blocked=true}},
    {{1,2}, #sector{food=0, blocked=false}},
    {{2,0}, #sector{food=1, blocked=false}},
    {{2,1}, #sector{food=0, blocked=true}},
    {{2,2}, #sector{food=0, blocked=false}}
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
