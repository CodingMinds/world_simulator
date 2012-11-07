%%%---------------------------------------------------------------------
%%% Description module world_env
%%%---------------------------------------------------------------------
%%% World_env holds the environment and the logic to interact with the
%%% modeled virtual world. This module uses the behaviour gen_server to
%%% provide the functionality and interfaces.
%%%---------------------------------------------------------------------
%%% Exports
%%% start_link(Map)
%%%   Starts the server and register the name ?MODULE.
%%%   Initialice a new virtual world with the map Map (see
%%%   world_records.hrl).
%%% init([Map])
%%%   Interface for the behaviour gen_server.
%%%   Initialice a new world with the map Map (see records.hrl).
%%% init([Map, Options])
%%%   Interface for the behaviour gen_server.
%%%   Initialice a new world with the map Map and the options Options
%%%   (see records.hrl).
%%% handle_call({load, Map}, From, World)
%%%   Interface for the behaviour gen_server.
%%%   Replace the actual running map with the new one (see records.hrl).
%%% handle_call({options, Options}, From, World)
%%%   Interface for the behaviour gen_server.
%%%   Set new options (see records.hrl).
%%% handle_call(map, From, World)
%%%   Interface for the behaviour gen_server.
%%%   Return the actual map as ASCII representation per row.
%%% handle_call(state, From, World)
%%%   Interface for the behaviour gen_server.
%%%   Return the actual internal state of the environment.
%%% handle_call(birth, From, World)
%%%   Interface for the behaviour gen_server.
%%%   Add the agent behind pid From to the map.
%%% handle_call(death, From, World)
%%%   Interface for the behaviour gen_server.
%%%   Remove the agent behind pid From from the map.
%%% handle_call({do, Action}, From, World)
%%%   Interface for the behaviour gen_server.
%%%   Try to fullfill the requested Action.
%%%---------------------------------------------------------------------

-module(world_env).
-author('M. Bittorf <info@coding-minds.com>').

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).
-export([start_link/1]).

-include("world_records.hrl").

%%----------------------------------------------------------------------
%% Function: start/1
%% Purpose: Calls gen_server:start_link/4 with map Map.
%% Args: The map Map which represents the modeled virtual world  (see
%%   world_records.hrl).
%% Returns: {ok,Pid} | ignore | {error,Error}
%%----------------------------------------------------------------------
start_link(Map) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Map], []).

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose: Interface for the behaviour gen_server.
%%   Initialice a new world with map Map.
%% Args: The map Map which represents the modeled virtual world and
%%   optional with the options record (see world_records.hrl).
%% Returns: {ok, World} with World as initial world
%%----------------------------------------------------------------------
init([Map]) when is_list(Map) ->
  World = #world{map = Map},
  {ok, World};

init([Map, Options]) when is_list(Map), is_record(Options, options) ->
  World = #world{map = Map, options = Options},
  {ok, World}.

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Replace the actual running map with the new one.
%% Args: The map Map which represents the modeled virtual world  (see
%%   world_records.hrl).
%% Returns: {reply, ok, #world}.
%%----------------------------------------------------------------------
handle_call({load, Map}, _From, #world{agents=Agents})
  when is_list(Map) ->
  lists:foreach(fun({Pid, _Coordinates}) ->
      gen_server:cast(Pid, world_destroyed)
    end, Agents),
  NewWorld = #world{map = Map, agents=[]},
  
  {reply, ok, NewWorld};

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Set a new set of options.
%% Args: The new options record (see world_records.hrl).
%% Returns: {reply, ok, #world}.
%%----------------------------------------------------------------------
handle_call({options, Options}, _From, World)
  when is_record(Options, options) ->
  
  NewWorld = World#world{options=Options},
  
  {reply, ok, NewWorld};

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Return the actual map.
%% Args: -
%% Returns: {reply, {map, Map}, #world}.
%%----------------------------------------------------------------------
handle_call(map, _From, World=#world{map=Map}) ->
  {reply, {map, Map}, World};

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Return the actual options.
%% Args: -
%% Returns: {reply, {options, ".."}, #world}.
%%----------------------------------------------------------------------
handle_call(options, _From, World=#world{options=Options}) ->
  {reply, {options, Options}, World};

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Return the actual internal world of the map.
%% Args: -
%% Returns: {reply, ok, #world}.
%%----------------------------------------------------------------------
handle_call(state, _From, World) ->
  {reply, {state, World}, World};

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Add the agent behind pid From to the map
%% Args: -
%% Returns: {reply, {ok, MapSize}, #world} | {reply, map_full, #world}.
%%----------------------------------------------------------------------
handle_call(birth, {Pid, _Tag},
  World=#world{options=Options, map=Map, agents=Agents}) ->
  if
    Options#options.max_agents > 0,
      length(Agents) >= Options#options.max_agents ->
      {reply, map_full, World};
    true ->
      Sector = world_helper:free_sector(Map),
      case Sector of
        false ->
          {reply, map_full, World};
        {Coordinates, Properties} ->
          NewMap = lists:keyreplace(Coordinates, 1, Map, {Coordinates,
                   Properties#sector{staffed=true}}),
          
          NewWorld = World#world{map = NewMap,
                     agents = Agents ++ [ {Pid, Coordinates} ]},
                
          {reply, ok, NewWorld}
      end
  end;

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Remove the agent behind pid From from the map.
%% Args: -
%% Returns: {reply, ok, #world}.
%%----------------------------------------------------------------------
handle_call(death, {Pid, _Tag}, World=#world{map=Map, agents=Agents}) ->
  % Free sector on map
  Agent = lists:keyfind(Pid, 1, Agents),
  NewMap = case Agent of
    false ->
      Map;
    _ ->
      {_Pid, Coordinates} = Agent,
      Result = lists:keyfind(Coordinates, 1, Map),
      case Result of
        false ->
          Map;
        {_, Sector} ->
          NewSector = {Coordinates, Sector#sector{staffed=false}},
          lists:keyreplace(Coordinates, 1, Map, NewSector)
      end
  end,
  
  % remove from agent list
  NewAgents = lists:keydelete(Pid, 1, Agents),
  
  NewWorld = World#world{map=NewMap, agents = NewAgents},
  
  {reply, ok, NewWorld};

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Handle incoming do requests. Check if they are valid,
%%   modifie the world and return the result to the client.
%% Args: -
%% Returns: {reply, ok, #world} | {reply, {food, Amount}, #world} |
%%   {reply, {error, Reason}, #world}.
%%----------------------------------------------------------------------
handle_call({do, Action}, {Pid, _Tag},
  World=#world{map=Map, agents=Agents}) ->
  %%--------------------------------------------------------------------
  %% Anonymous function: fun/2
  %% Purpose: Check if the applied sector is available for the client
  %%   and set the new position if possible
  %% Args: Actual and target sections.
  %% Returns: {reply, ok, World} | {reply, {food, N}, World} | {reply,
  %%   {Error, Reason}, World}
  %%--------------------------------------------------------------------
  CheckAndApply = fun({CoordinatesNow, SectorNow},
    {CoordinatesNew, SectorNew}) ->
    if
      SectorNew#sector.blocked == true ->
        {reply, {error, blocked}, World};
      SectorNew#sector.staffed == true ->
        {reply, {error, staffed}, World};
      SectorNew#sector.food /= 0 ->
        NewAgents = lists:keyreplace(Pid, 1, Agents,
                    {Pid, CoordinatesNew}),
        NewMap = lists:keyreplace(CoordinatesNow, 1, Map,
                 {CoordinatesNow, SectorNow#sector{staffed=false}}),
        NewMap2 = lists:keyreplace(CoordinatesNew, 1, NewMap,
                  {CoordinatesNew, SectorNew#sector{staffed=true}}),
        NewWorld = world_helper:consume_food(
          CoordinatesNew, World#world{map=NewMap2, agents=NewAgents}),
        
        {reply, {food, SectorNew#sector.food}, NewWorld};
      true ->
        NewAgents = lists:keyreplace(Pid, 1, Agents,
                    {Pid, CoordinatesNew}),
        NewMap = lists:keyreplace(CoordinatesNow, 1, Map,
                 {CoordinatesNow, SectorNow#sector{staffed=false}}),
        NewMap2 = lists:keyreplace(CoordinatesNew, 1, NewMap,
                  {CoordinatesNew, SectorNew#sector{staffed=true}}),
        NewWorld = World#world{map=NewMap2, agents=NewAgents},
        
        {reply, ok, NewWorld}
    end
  end,
  
  case lists:keyfind(Pid, 1, Agents) of
    false ->
      {reply, {error, client_unknown}, World};
    {_Pid, {X, Y}} ->
      case Action of
        {move, Direction} ->
          CurrentSector = world_helper:get_sector(X, Y, Map),
          case Direction of
            1 ->
              CheckAndApply(CurrentSector,
                world_helper:get_sector(X, Y-1, Map));
            2 ->
              CheckAndApply(CurrentSector,
                world_helper:get_sector(X+1, Y-1, Map));
            3 ->
              CheckAndApply(CurrentSector,
                world_helper:get_sector(X+1, Y, Map));
            4 ->
              CheckAndApply(CurrentSector,
                world_helper:get_sector(X+1, Y+1, Map));
            5 ->
              CheckAndApply(CurrentSector,
                world_helper:get_sector(X, Y+1, Map));
            6 ->
              CheckAndApply(CurrentSector,
                world_helper:get_sector(X-1, Y+1, Map));
            7 ->
              CheckAndApply(CurrentSector,
                world_helper:get_sector(X-1, Y, Map));
            8 ->
              CheckAndApply(CurrentSector,
                world_helper:get_sector(X-1, Y-1, Map));
            _ ->
              {reply, {error, bad_arg}, World}
          end;
        environ ->
          Environ = [
            world_helper:ascii_rep(
              world_helper:get_sector(X,   Y-1, Map)),
            world_helper:ascii_rep(
              world_helper:get_sector(X+1, Y-1, Map)),
            world_helper:ascii_rep(
              world_helper:get_sector(X+1, Y,   Map)),
            world_helper:ascii_rep(
              world_helper:get_sector(X+1, Y+1, Map)),
            world_helper:ascii_rep(
              world_helper:get_sector(X,   Y+1, Map)),
            world_helper:ascii_rep(
              world_helper:get_sector(X-1, Y+1, Map)),
            world_helper:ascii_rep(
              world_helper:get_sector(X-1, Y,   Map)),
            world_helper:ascii_rep(
              world_helper:get_sector(X-1, Y-1, Map))
          ],
          
          {reply, {environ, Environ}, World};
        _ ->
          {reply, {error, command_unknown}, World}
      end
  end.

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Stops the scheduler
%% Args: -
%% Returns: {stop, normal, World}.
%%----------------------------------------------------------------------
handle_cast(stop, World) ->
  {stop, normal, World}.
  
%%----------------------------------------------------------------------
%% Function: *
%% Purpose: Dummy functions for the behaviour gen_server
%%----------------------------------------------------------------------
terminate(normal, _World) -> ok;
terminate(_Reason, _World) -> ok.
handle_info(_Message, World) -> {noreply, World}.
code_change(_OldVersion, World, _Extra) -> {ok, World}.