%%%---------------------------------------------------------------------
%%% Description module world
%%%---------------------------------------------------------------------
%%% World holds all the states and the logic to interact with the
%%% modeled virtual world. This module uses the behaviour gen_server to
%%% provide the functionality and interfaces.
%%%---------------------------------------------------------------------
%%% Exports
%%% start(Map)
%%%   Starts the server and register the name ?MODULE.
%%%   Initialice a new world with the map Map (see records.hrl).
%%% stop()
%%%   Stops the server.
%%% init([Map])
%%%   Interface for the behaviour gen_server.
%%%   Initialice a new world with the map Map (see records.hrl).
%%% handle_call({load, Map}, From, State)
%%%   Interface for the behaviour gen_server.
%%%   Replace the actual running map with the new one (see records.hrl).
%%% handle_call(state, From, State)
%%%   Interface for the behaviour gen_server.
%%%   Return the actual internal state of the map.
%%% handle_call(birth, From, State)
%%%   Interface for the behaviour gen_server.
%%%   Add the agent behind pid From to the map.
%%% handle_call(death, From, State)
%%%   Interface for the behaviour gen_server.
%%%   Remove the agent behind pid From from the map.
%%% handle_call({do, Action}, From, State)
%%%   Interface for the behaviour gen_server.
%%%   Try to fullfill the requested Action.
%%%---------------------------------------------------------------------

-module(world).
-author('M. Bittorf <info@coding-minds.com>').

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, stop/0]).

-include("records.hrl").

%%----------------------------------------------------------------------
%% Function: start/1
%% Purpose: Calls gen_server:start_link/4 with map Map.
%% Args: The map Map which represents the modeled virtual world  (see
%%   records.hrl).
%% Returns: {ok,Pid} | ignore | {error,Error}
%%----------------------------------------------------------------------
start(Map) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Map], []).

%%----------------------------------------------------------------------
%% Function: stop/0
%% Purpose: Stops the gen_server
%% Args: -
%% Returns: ok.
%%----------------------------------------------------------------------
stop() -> gen_server:cast(?MODULE, stop).

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose: Interface for the behaviour gen_server.
%%   Initialice a new world with map Map.
%% Args: The map Map which represents the modeled virtual world  (see
%%   records.hrl).
%% Returns: {ok, State} with State as initial state
%%----------------------------------------------------------------------
init([Map]) when is_list(Map) ->
  State = #state{map = Map},
  {ok, State}.

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Replace the actual running map with the new one.
%% Args: The map Map which represents the modeled virtual world  (see
%%   records.hrl).
%% Returns: {reply, ok, #state}.
%%----------------------------------------------------------------------
handle_call({load, Map}, _From, _State) ->
  NewState = #state{map = Map},
  {reply, ok, NewState};

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Return the actual internal state of the map.
%% Args: -
%% Returns: {reply, ok, #state}.
%%----------------------------------------------------------------------
handle_call(state, _From, State) ->
  {reply, State, State};

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Add the agent behind pid From to the map
%% Args: -
%% Returns: {reply, {ok, MapSize}, #state} | {reply, map_full, #state}.
%%----------------------------------------------------------------------
handle_call(birth, {Pid, _Tag}, State=#state{map=Map, agents=Agents}) ->
  Sector = free_sector(Map),
  case Sector of
    false ->
      {reply, map_full, State};
    {Coordinates, Properties} ->
      
      NewMap = lists:keyreplace(Coordinates, 1, Map, {Coordinates, Properties#sector{staffed=true}}),
      
      NewState = State#state{map = NewMap, agents = Agents ++ [ {Pid, Coordinates} ]},
            
      {reply, ok, NewState}
  end;

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Remove the agent behind pid From from the map.
%% Args: -
%% Returns: {reply, ok, #state}.
%%----------------------------------------------------------------------
handle_call(death, {Pid, _Tag}, State=#state{map=Map, agents=Agents}) ->
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
  
  NewState = State#state{map=NewMap, agents = NewAgents},
  
  {reply, ok, NewState};

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Handle incoming do requests. Check if they are valid,
%%   modifie the world and return the result to the client.
%% Args: -
%% Returns: {reply, ok, #state} | {reply, {food, Amount}, #state} |
%%   {reply, {error, Reason}, #state}.
%%----------------------------------------------------------------------
handle_call({do, Action}, {Pid, _Tag}, State=#state{map=Map, agents=Agents}) ->
  %%--------------------------------------------------------------------
  %% Anonymous function: fun/2
  %% Purpose: Check if the applied sector is available for the client
  %%   and set the new position if possible
  %% Args: Actual and target sections.
  %% Returns: {reply, ok, State} | {reply, {food, N}, State} | {reply,
  %%   {Error, Reason}, State}
  %%--------------------------------------------------------------------
  CheckAndApply = fun({CoordinatesNow, SectorNow}, {CoordinatesNew, SectorNew}) ->
    if
      SectorNew#sector.blocked == true ->
        {reply, {error, blocked}, State};
      SectorNew#sector.staffed == true ->
        {reply, {error, staffed}, State};
      SectorNew#sector.food /= 0 ->
        NewAgents = lists:keyreplace(Pid, 1, Agents, {Pid, CoordinatesNew}),
        NewMap = lists:keyreplace(CoordinatesNow, 1, Map, {CoordinatesNow, SectorNow#sector{staffed=false}}),
        NewMap2 = lists:keyreplace(CoordinatesNew, 1, NewMap, {CoordinatesNew, SectorNew#sector{staffed=true}}),
        NewState = State#state{map=NewMap2, agents=NewAgents},
        
        {reply, {food, SectorNew#sector.food}, NewState};
      true ->
        NewAgents = lists:keyreplace(Pid, 1, Agents, {Pid, CoordinatesNew}),
        NewMap = lists:keyreplace(CoordinatesNow, 1, Map, {CoordinatesNow, SectorNow#sector{staffed=false}}),
        NewMap2 = lists:keyreplace(CoordinatesNew, 1, NewMap, {CoordinatesNew, SectorNew#sector{staffed=true}}),
        NewState = State#state{map=NewMap2, agents=NewAgents},
        
        {reply, ok, NewState}
    end
  end,
  
  %%--------------------------------------------------------------------
  %% Anonymous function: fun/1
  %% Purpose: Return character which represents the state of the given
  %%   sector. Based on Wilson's WOOD1
  %% Args: Sector.
  %% Returns: . | O  | F | *
  %%--------------------------------------------------------------------
  GetASCIIRepresentation = fun({_, Sector}) ->
    if
      Sector#sector.blocked == true ->
        "O";
      Sector#sector.staffed == true ->
        "*";
      Sector#sector.food /= 0 ->
        "F";
      true ->
        "."
    end
  end,
  
  case lists:keyfind(Pid, 1, Agents) of
    false ->
      {reply, {error, client_unknown}, State};
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
              {reply, {error, bad_arg}, State}
          end;
        environ ->
          Environ = [
            GetASCIIRepresentation(get_sector(X,   Y-1, Map)),
            GetASCIIRepresentation(get_sector(X+1, Y-1, Map)),
            GetASCIIRepresentation(get_sector(X+1, Y,   Map)),
            GetASCIIRepresentation(get_sector(X+1, Y+1, Map)),
            GetASCIIRepresentation(get_sector(X,   Y+1, Map)),
            GetASCIIRepresentation(get_sector(X-1, Y+1, Map)),
            GetASCIIRepresentation(get_sector(X-1, Y,   Map)),
            GetASCIIRepresentation(get_sector(X-1, Y-1, Map))
          ],
          
          {reply, {environ, Environ}, State};
        _ ->
          {reply, {error, command_unknown}, State}
      end
  end.

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Stops the scheduler
%% Args: -
%% Returns: {stop, normal, State}.
%%----------------------------------------------------------------------
handle_cast(stop, State) ->
  {stop, normal, State}.
  
%%----------------------------------------------------------------------
%% Function: *
%% Purpose: Dummy functions for the behaviour gen_server
%%----------------------------------------------------------------------
terminate(normal, _State) -> ok;
terminate(_Reason, _State) -> ok.
handle_info(_Message, State) -> {noreply, State}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%----------------------------------------------------------------------
%% Function: free_sector/1
%% Purpose: Return the coordinates of the next free sector or false
%% Args: Map as map which should be used.
%% Returns: {{X, Y}, #sector} | false.
%%----------------------------------------------------------------------
free_sector(Map) ->
  FreeSectors = lists:filter(fun({_, #sector{staffed=Staffed}}) -> Staffed == false end, Map),
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
  case lists:filter(fun({{Xm, Ym}, _}) -> (X==Xm) and (Y==Ym) end, Map) of
    [] ->
      {{X, Y}, #sector{blocked=true}};
    SectorList ->
      lists:nth(1, SectorList)
  end.
