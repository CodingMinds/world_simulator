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
%%% handle_call({load, Map}, From, World)
%%%   Interface for the behaviour gen_server.
%%%   Replace the actual running map with the new one (see records.hrl).
%%% handle_call(state, From, World)
%%%   Interface for the behaviour gen_server.
%%%   Return the actual internal world of the map.
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
%% Args: The map Map which represents the modeled virtual world  (see
%%   world_records.hrl).
%% Returns: {ok, World} with World as initial world
%%----------------------------------------------------------------------
init([Map]) when is_list(Map) ->
  World = #world{map = Map},
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
%% Purpose: Show the actual map as ASCII representation.
%% Args: -
%% Returns: {reply, {map, AsciiMap}, #world}.
%%----------------------------------------------------------------------
handle_call(map, _From, World=#world{map=Map}) ->
  %% Get max size of map
  MapCoordinates = lists:map(fun({Coordinates, _}) ->
                     Coordinates end, Map),
  {X, Y} = lists:nth(1, lists:sort(
                          fun({Xa, Ya}, {Xb, Yb}) ->
                            (Xb < Xa) and (Yb < Ya)
                          end,
                        MapCoordinates)),
  
  %% create ascii representation based on max size
  AsciiMap = lists:foldl(
               fun(Yi, RowAcc) ->
                 Col = lists:foldl(
                         fun(Xi, ColAcc) ->
                           ColAcc ++ ascii_rep(get_sector(Xi, Yi, Map))
                         end,
                       [], lists:seq(1, X)),
                 if
                   Yi /= Y ->
                     RowAcc ++ Col ++ "~n";
                   true ->
                     RowAcc ++ Col
                 end
               end,
             [], lists:seq(1, Y)),
  
  {reply, {map, AsciiMap}, World};

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
handle_call(birth, {Pid, _Tag}, World=#world{map=Map, agents=Agents}) ->
  Sector = free_sector(Map),
  case Sector of
    false ->
      {reply, map_full, World};
    {Coordinates, Properties} ->
      
      NewMap = lists:keyreplace(Coordinates, 1, Map, {Coordinates,
               Properties#sector{staffed=true}}),
      
      NewWorld = World#world{map = NewMap,
                 agents = Agents ++ [ {Pid, Coordinates} ]},
            
      {reply, ok, NewWorld}
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
        NewWorld = World#world{map=NewMap2, agents=NewAgents},
        
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
          CurrentSector = get_sector(X, Y, Map),
          case Direction of
            1 ->
              CheckAndApply(CurrentSector, get_sector(X, Y-1, Map));
            2 ->
              CheckAndApply(CurrentSector, get_sector(X+1, Y-1, Map));
            3 ->
              CheckAndApply(CurrentSector, get_sector(X+1, Y, Map));
            4 ->
              CheckAndApply(CurrentSector, get_sector(X+1, Y+1, Map));
            5 ->
              CheckAndApply(CurrentSector, get_sector(X, Y+1, Map));
            6 ->
              CheckAndApply(CurrentSector, get_sector(X-1, Y+1, Map));
            7 ->
              CheckAndApply(CurrentSector, get_sector(X-1, Y, Map));
            8 ->
              CheckAndApply(CurrentSector, get_sector(X-1, Y-1, Map));
            _ ->
              {reply, {error, bad_arg}, World}
          end;
        environ ->
          Environ = [
            ascii_rep(get_sector(X,   Y-1, Map)),
            ascii_rep(get_sector(X+1, Y-1, Map)),
            ascii_rep(get_sector(X+1, Y,   Map)),
            ascii_rep(get_sector(X+1, Y+1, Map)),
            ascii_rep(get_sector(X,   Y+1, Map)),
            ascii_rep(get_sector(X-1, Y+1, Map)),
            ascii_rep(get_sector(X-1, Y,   Map)),
            ascii_rep(get_sector(X-1, Y-1, Map))
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

%%----------------------------------------------------------------------
%% Function: free_sector/1
%% Purpose: Return the coordinates of the next free sector or false
%% Args: Map as map which should be used.
%% Returns: {{X, Y}, #sector} | false.
%%----------------------------------------------------------------------
free_sector(Map) ->
  FreeSectors = lists:filter(fun({_, #sector{staffed=Staffed}}) ->
                             Staffed == false end, Map),
  case FreeSectors of
    [] ->
      false;
    _ ->
      [ FreeSector | _ ] = FreeSectors,
      FreeSector
  end.

%%----------------------------------------------------------------------
%% Function: get_sector/3
%% Purpose: Get the sector on position X and Y of map Map. Return a
%%  blocked one if there is no sector on this position.
%% Args: X and Y as coordinates on map Map.
%% Returns: {{X, Y}, #sector}
%%----------------------------------------------------------------------
get_sector(X, Y, Map) ->
  case lists:filter(fun({{Xm, Ym}, _}) ->
                    (X==Xm) and (Y==Ym) end, Map) of
    [] ->
      {{X, Y}, #sector{blocked=true}};
    SectorList ->
      lists:nth(1, SectorList)
  end.

%%----------------------------------------------------------------------
%% Function: ascii_rep/1
%% Purpose: Return character which represents the world of the given
%%   sector. Based on Wilson's WOOD1
%% Args: Sector.
%% Returns: . | O  | F | *
%%----------------------------------------------------------------------
ascii_rep({_, Sector}) ->
  if
    Sector#sector.blocked == true ->
      "O";
    Sector#sector.staffed == true ->
      "*";
    Sector#sector.food /= 0 ->
      "F";
    true ->
      "."
  end.
