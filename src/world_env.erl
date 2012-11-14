%%%---------------------------------------------------------------------
%%% Description module world_env
%%%---------------------------------------------------------------------
%%% @author M. Bittorf <info@coding-minds.com>
%%% @copyright 2012 M. Bittorf
%%% @doc {@module} holds the environment and the logic to interact with
%%% the modeled virtual world. This module uses the behaviour gen_server
%%% to provide the functionality and interfaces.
%%% @end
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
%%% handle_call({map, Map}, From, World)
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
%%%   Try to fulfill the requested Action.
%%%---------------------------------------------------------------------

-module(world_env).
-author('M. Bittorf <info@coding-minds.com>').

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).
-export([start_link/1]).

-include("world_records.hrl").

%%----------------------------------------------------------------------
%% Function: start_link/1
%% Purpose: Calls gen_server:start_link/4 with map Map.
%% Args: The map Map which represents the modeled virtual world  (see
%%   world_records.hrl).
%% Returns: {ok,Pid} | ignore | {error,Error}
%%----------------------------------------------------------------------
%% @doc Wrapper for start_link of gen_server.
start_link(Map) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Map], []).

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose: Interface for the behaviour gen_server.
%%   Initialize a new world with map Map.
%% Args: The map Map which represents the modeled virtual world and
%%   optional with the options record (see world_records.hrl).
%% Returns: {ok, World} with World as initial world
%%----------------------------------------------------------------------
%% @doc Interface for the behaviour gen_server.
%%   Initialize a new world with map `Map' and an optional options
%%   record `Options'.
init([Map]) when is_list(Map) ->
  World = #world{map = Map},
  AsciiRows = world_helper:map_to_ascii(Map),
  world_helper:log(env, "World started with map ~n" ++
    string:join(AsciiRows, "~n")),
  
  {ok, World};

init([Map, Options]) when is_list(Map), is_record(Options, options) ->
  World = #world{map = Map, options = Options},
  AsciiRows = world_helper:map_to_ascii(Map),
  AsciiOptions = world_helper:options_to_ascii(Options),
  world_helper:log(env, "World started with map ~n" ++
    string:join(AsciiRows, "~n") ++ "~n and options ~n" ++
    string:join(AsciiOptions, "~n")),
  
  {ok, World}.

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Replace the actual running map with the new one.
%% Args: The map Map which represents the modeled virtual world  (see
%%   world_records.hrl).
%% Returns: {reply, ok, #world}.
%%----------------------------------------------------------------------
%% @doc Interface for the behaviour gen_server.
%%   Handle incoming interactions with the world.
handle_call({map, Map}, _From, World=#world{agents=Agents})
  when is_list(Map) ->
  lists:foreach(fun({Pid, _Coordinates}) ->
      gen_server:cast(Pid, world_destroyed)
    end, Agents),
  NewWorld = World#world{map = Map, agents=[]},
  
  AsciiRows = world_helper:map_to_ascii(Map),
  world_helper:log(env, "Loaded map ~n" ++
    string:join(AsciiRows, "~n")),
  
  {reply, ok, NewWorld};

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Set a new set of options.
%% Args: The new options record (see world_records.hrl).
%% Returns: {reply, ok, #world}.
%%----------------------------------------------------------------------
handle_call({options, Options}, _From, World=#world{agents=Agents})
  when is_record(Options, options) ->
  
  NewWorld = World#world{options=Options},
  
  AsciiOptions = world_helper:options_to_ascii(Options),
  world_helper:log(env, "Loaded options ~n" ++
    string:join(AsciiOptions, "~n")),
  
  % send broadcast to all clients
  lists:foreach(fun({Pid, _Coordinates}) ->
    gen_server:cast(Pid, world_changed)
  end, Agents),
  
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
                   Properties#sector{staffed=1+length(Agents)}}),
          
          NewWorld = World#world{map = NewMap,
                     agents = Agents ++ [ {Pid, Coordinates} ]},
           
           world_helper:log(client, "Client ~w entered world", [Pid]),
           
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
          NewSector = {Coordinates, Sector#sector{staffed=0}},
          lists:keyreplace(Coordinates, 1, Map, NewSector)
      end
  end,
  
  % remove from agent list
  NewAgents = lists:keydelete(Pid, 1, Agents),
  
  NewWorld = World#world{map=NewMap, agents = NewAgents},
  
  world_helper:log(client, "Client ~w left world", [Pid]),
  
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
  %% Anonymous function: fun/1
  %% Purpose: Return the environ String for the position tuple on map
  %%   BaseMap
  %% Args: Position as tuple {x, y} and the map as BaseMap.
  %% Returns: String
  %%--------------------------------------------------------------------
  GetEnvironString = fun({X, Y}, BaseMap) ->
    [
      world_helper:ascii_rep(
        world_helper:get_sector(X,   Y-1, BaseMap)),
      world_helper:ascii_rep(
        world_helper:get_sector(X+1, Y-1, BaseMap)),
      world_helper:ascii_rep(
        world_helper:get_sector(X+1, Y,   BaseMap)),
      world_helper:ascii_rep(
        world_helper:get_sector(X+1, Y+1, BaseMap)),
      world_helper:ascii_rep(
        world_helper:get_sector(X,   Y+1, BaseMap)),
      world_helper:ascii_rep(
        world_helper:get_sector(X-1, Y+1, BaseMap)),
      world_helper:ascii_rep(
        world_helper:get_sector(X-1, Y,   BaseMap)),
      world_helper:ascii_rep(
        world_helper:get_sector(X-1, Y-1, BaseMap))
    ]
  end,
  
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
        world_helper:log(client, "Client ~w tried ~w: blocked",
          [Pid, CoordinatesNew]),
        
        {reply, {error, blocked}, World};
      SectorNew#sector.staffed /= 0 ->
        world_helper:log(client, "Client ~w tried ~w: staffed",
          [Pid, CoordinatesNew]),
        
        {reply, {error, staffed}, World};
      true ->
        NewAgents = lists:keyreplace(Pid, 1, Agents,
                    {Pid, CoordinatesNew}),
        NewMap = lists:keyreplace(CoordinatesNow, 1, Map,
                 {CoordinatesNow, SectorNow#sector{staffed=0}}),
        AgentPos = string:str(NewAgents, [{Pid, CoordinatesNew}]),
        NewMap2 = lists:keyreplace(CoordinatesNew, 1, NewMap,
                  {CoordinatesNew, SectorNew#sector{staffed=AgentPos}}),
        
        if
          SectorNew#sector.food /= 0 ->
            NewWorld = world_helper:consume_food(CoordinatesNew,
              World#world{map=NewMap2, agents=NewAgents}),
            
            world_helper:log(client, "Client ~w tried ~w: food ~B",
              [Pid, CoordinatesNew, SectorNew#sector.food]),
            
            {reply, {food, SectorNew#sector.food}, NewWorld};
          true ->
            NewWorld = World#world{map=NewMap2, agents=NewAgents},
            
            Environ = GetEnvironString(CoordinatesNew, NewMap2),
            
            world_helper:log(client, "Client ~w tried ~w: success",
              [Pid, CoordinatesNew]),
              
            {reply, {ok, Environ}, NewWorld}
        end
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
            0 ->
              world_helper:log(client, "Client ~w hasn't moved",
                [Pid]),
              {reply, ok, World};
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
          Environ = GetEnvironString({X, Y}, Map),
          
          {reply, {environ, Environ}, World};
        _ ->
          {reply, {error, command_unknown}, World}
      end
  end.

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Stops the environment.
%% Args: -
%% Returns: {stop, normal, World}.
%%----------------------------------------------------------------------
%% @doc Interface for the behaviour gen_server.
%%   Only used to stop the environment
handle_cast(stop, World) ->
  {stop, normal, World}.
  
%%----------------------------------------------------------------------
%% Function: *
%% Purpose: Dummy functions for the behaviour gen_server
%%----------------------------------------------------------------------
%% @doc Dummy function for the behaviour gen_server. Not used in this
%% implementation
terminate(normal, _World) -> ok;
terminate(_Reason, _World) -> ok.
%% @doc Dummy function for the behaviour gen_server. Not used in this
%% implementation
handle_info(_Message, World) -> {noreply, World}.
%% @doc Dummy function for the behaviour gen_server. Not used in this
%% implementation
code_change(_OldVersion, World, _Extra) -> {ok, World}.