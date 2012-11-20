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
%%% start_link(Arguments)
%%%   Starts the server and register the name ?MODULE.
%%%   Initialize a new virtual world with the arguments list Arguments.
%%% init([])
%%%   Interface for the behaviour gen_server.
%%%   Initialize a new world with an empty map.
%%% init([Map])
%%%   Interface for the behaviour gen_server.
%%%   Initialize a new world with the map Map (see records.hrl).
%%% init([Map, Options])
%%%   Interface for the behaviour gen_server.
%%%   Initialize a new world with the map Map and the options Options
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
%%% handle_call({birth, X, Y}, From, World)
%%%   Interface for the behaviour gen_server.
%%%   Add the agent behind pid From to the map. If X and Y /= -1 try to
%%%   set the agent on position X,Y
%%% handle_call(dead, From, World)
%%%   Interface for the behaviour gen_server.
%%%   Remove the agent behind pid From from the map.
%%% handle_call({do, Action}, From, World)
%%%   Interface for the behaviour gen_server.
%%%   Try to fulfill the requested Action.
%%% handle_call({drop, all},World)
%%%   Interface for the behaviour gen_server.
%%%   Drops all agents from the map.
%%% handle_call({drop, dead},World)
%%%   Interface for the behaviour gen_server.
%%%   Drops all dead agents from the map.
%%% handle_cast(stop, From, World)
%%%   Interface for the behaviour gen_server.
%%%   Destroy the world.
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
%% Purpose: Calls gen_server:start_link/4 with the arguments list
%%   Arguments.
%% Args: The arguments Arguments which shall be passed to init/1
%% Returns: {ok,Pid} | ignore | {error,Error}
%%----------------------------------------------------------------------
%% @doc Wrapper for start_link of gen_server.
start_link(Arguments) ->
  gen_server:start_link(?MODULE, Arguments, []).

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose: Interface for the behaviour gen_server.
%%   Initialize a new world with map Map if available.
%% Args: The optional map Map which represents the modeled virtual world
%%   and the optional options record (see world_records.hrl).
%% Returns: {ok, World} with World as initial world
%%----------------------------------------------------------------------
%% @doc Interface for the behaviour gen_server.
%%   Initialize a new world with the optional map `Map' and an optional
%%   options record `Options'.
init([]) ->
  world_helper:log(env, "World started with an empty map"),
  {ok, #world{}};

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
  lists:foreach(fun({Pid, _Coordinates, _Energy}) ->
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
handle_call({options, Options}, _From, World)
  when is_record(Options, options) ->
  
  NewWorld = World#world{options=Options},
  
  AsciiOptions = world_helper:options_to_ascii(Options),
  world_helper:log(env, "Loaded options ~n" ++
    string:join(AsciiOptions, "~n")),
  
  % send broadcast to all clients - deprecated
  %lists:foreach(fun({Pid, _Coordinates, _Energy}) ->
  %  gen_server:cast(Pid, world_changed)
  %end, Agents),
  
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
%% Returns: {reply, {state, World}, #world}.
%%----------------------------------------------------------------------
handle_call(state, _From, World) ->
  {reply, {state, World}, World};

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Return a tuple with the name and size of the environment,
%%   the agent count and the max amount of agents
%% Args: -
%% Returns: {reply, {info, Name, X, Y, Agents, MaxAgents}, #world}.
%%----------------------------------------------------------------------
handle_call(info, _From, World=#world{map=Map, options=Opt,
  agents=Agents}) ->
  {X, Y} = world_helper:map_size(Map),
  
  {reply, {info, Opt#options.env_name, X, Y, length(Agents),
    Opt#options.max_agents}, World};

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Add the agent behind pid From to the map on the desired
%%   position X,Y. If X or Y is 0 they will be ignored.
%% Args: -
%% Returns: {reply, {ok, MapSize}, #world} | {reply, map_full, #world}
%%    | {reply, invalid_position, #world}
%%    | {reply, access_denied, #world}.
%%----------------------------------------------------------------------
handle_call({birth, X, Y}, {Pid, _Tag},
  World=#world{options=Options, map=Map, agents=Agents}) ->
  if
    Options#options.max_agents > 0,
      length(Agents) >= Options#options.max_agents ->
      {reply, map_full, World};
    true ->
      Sector = world_helper:birth_sector(World, X, Y),
      case Sector of
        {error, map_full} ->
          {reply, map_full, World};
        {error, invalid_position} ->
          {reply, invalid_position, World};
        {error, access_denied} ->
          {reply, access_denied, World};
        {Coordinates, Properties} ->
          
          NewMap = lists:keyreplace(Coordinates, 1, Map, {Coordinates,
                   Properties#sector{staffed=1+length(Agents)}}),
          
          NewWorld = World#world{map = NewMap,
                     agents = Agents ++ [ {Pid, Coordinates,
                       Options#options.initial_energy} ]},
           
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
handle_call(dead, {Pid, _Tag}, World) ->
  NewWorld = remove_agent(Pid, World),
  
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
  World=#world{map=Map, agents=Agents, options=Options}) ->
  %%--------------------------------------------------------------------
  %% Anonymous function: fun/1
  %% Purpose: Return the environ String for the position tuple on map
  %%   BaseMap
  %% Args: Position as tuple {x, y}, the BaseMap as map and Fitness.
  %% Returns: String
  %%--------------------------------------------------------------------
  GetEnvironString = fun({X, Y}, BaseMap, Fitness) ->
    integer_to_list(Fitness) ++ ":" ++
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
  %%   and set the new position if possible. Also recalculates Energy.
  %% Args: Actual and target sections and actual Energy of the Agent.
  %% Returns: {Result, Energy, World}
  %%--------------------------------------------------------------------
  CheckAndApply = fun({CoordinatesNow, SectorNow},
    {CoordinatesNew, SectorNew}, Energy) ->
    if
      CoordinatesNow == CoordinatesNew ->
        NewEnergy = Energy - Options#options.energy_nomove,
        NewAgents = lists:keyreplace(Pid, 1, Agents,
          {Pid, CoordinatesNew, NewEnergy}),
        NewWorld = World#world{agents=NewAgents},
        
        world_helper:log(client, "Client ~w hasn't moved", [Pid]),
        
        Environ = GetEnvironString(CoordinatesNew, Map,
          Options#options.fitness_nomove),
        
        {{ok, Environ}, NewEnergy, NewWorld};
      
      SectorNew#sector.blocked == true ->
        NewEnergy = Energy - Options#options.energy_blocked,
        NewAgents = lists:keyreplace(Pid, 1, Agents,
          {Pid, CoordinatesNow, NewEnergy}),
        NewWorld = World#world{agents=NewAgents},
        
        world_helper:log(client, "Client ~w tried ~w: blocked",
          [Pid, CoordinatesNew]),
        
        Environ = GetEnvironString(CoordinatesNow, NewWorld#world.map,
          Options#options.fitness_blocked),
        
        {{error, blocked, Environ}, NewEnergy, NewWorld};
      
      SectorNew#sector.staffed /= 0 ->
        NewEnergy = Energy - Options#options.energy_staffed,
        NewAgents = lists:keyreplace(Pid, 1, Agents,
          {Pid, CoordinatesNow, NewEnergy}),
        NewWorld = World#world{agents=NewAgents},
        
        world_helper:log(client, "Client ~w tried ~w: staffed",
          [Pid, CoordinatesNew]),
        
        Environ = GetEnvironString(CoordinatesNow, NewWorld#world.map,
          Options#options.fitness_staffed),
        
        {{error, staffed, Environ}, NewEnergy, NewWorld};
      
      true ->
        NewMap = lists:keyreplace(CoordinatesNow, 1, Map,
          {CoordinatesNow, SectorNow#sector{staffed=0}}),
        AgentPos = string:str(Agents, [{Pid, CoordinatesNow,
          Energy}]),
        NewMap2 = lists:keyreplace(CoordinatesNew, 1, NewMap,
          {CoordinatesNew, SectorNew#sector{staffed=AgentPos}}),
        
        if
          SectorNew#sector.food /= 0 ->
            NewEnergy = Energy + SectorNew#sector.food,
            NewAgents = lists:keyreplace(Pid, 1, Agents,
              {Pid, CoordinatesNew, NewEnergy}),
            NewWorld = world_helper:consume_food(CoordinatesNew,
              World#world{map=NewMap2, agents=NewAgents}),
            
            world_helper:log(client, "Client ~w tried ~w: food ~B",
              [Pid, CoordinatesNew, SectorNew#sector.food]),
            
            Environ = GetEnvironString(CoordinatesNew, NewMap2,
              SectorNew#sector.food),
            
            {{food, Environ}, NewEnergy, NewWorld};
          
          true ->
            NewEnergy = Energy - Options#options.energy_moved,
            NewAgents = lists:keyreplace(Pid, 1, Agents,
              {Pid, CoordinatesNew, NewEnergy}),
            NewWorld = World#world{map=NewMap2, agents=NewAgents},
            
            world_helper:log(client, "Client ~w tried ~w: success",
              [Pid, CoordinatesNew]),
            
            Environ = GetEnvironString(CoordinatesNew, NewMap2,
              Options#options.fitness_moved),
            
            {{ok, Environ}, NewEnergy, NewWorld}
        end
    end
  end,
  
  case lists:keyfind(Pid, 1, Agents) of
    false ->
      {reply, {error, client_unknown}, World};
    {_Pid, {X, Y}, Energy} ->
      case Action of
        {move, Direction} ->
          CurrentSector = world_helper:get_sector(X, Y, Map),
          {Result, NewEnergy, NewWorld} = case Direction of
            0 ->
              CheckAndApply(CurrentSector, CurrentSector, Energy);
            1 ->
              CheckAndApply(CurrentSector,
                world_helper:get_sector(X, Y-1, Map), Energy);
            2 ->
              CheckAndApply(CurrentSector,
                world_helper:get_sector(X+1, Y-1, Map), Energy);
            3 ->
              CheckAndApply(CurrentSector,
                world_helper:get_sector(X+1, Y, Map), Energy);
            4 ->
              CheckAndApply(CurrentSector,
                world_helper:get_sector(X+1, Y+1, Map), Energy);
            5 ->
              CheckAndApply(CurrentSector,
                world_helper:get_sector(X, Y+1, Map), Energy);
            6 ->
              CheckAndApply(CurrentSector,
                world_helper:get_sector(X-1, Y+1, Map), Energy);
            7 ->
              CheckAndApply(CurrentSector,
                world_helper:get_sector(X-1, Y, Map), Energy);
            8 ->
              CheckAndApply(CurrentSector,
                world_helper:get_sector(X-1, Y-1, Map), Energy);
            _ ->
              {reply, {error, bad_arg}, World}
          end,
          
          if
            Options#options.drop_agents and (NewEnergy =< 0) ->
              NewWorld2 = remove_agent(Pid, NewWorld),
              {reply, {error, dead}, NewWorld2};
            true ->
              {reply, Result, NewWorld}
          end;
        environ ->
          Environ = GetEnvironString({X, Y}, Map, 0),
          
          {reply, {environ, Environ}, World};
        _ ->
          {reply, {error, command_unknown}, World}
      end
  end;

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Drops all agents from the map.
%% Args: -
%% Returns: {reply, ok, World}.
%%----------------------------------------------------------------------
handle_call({drop, all}, _From, World=#world{agents=Agents}) ->
  % send broadcast to all clients
  lists:foreach(fun({Pid, _Coordinates, _Energy}) ->
    gen_server:cast(Pid, dead)
  end, Agents),
  
  {reply, ok, World};

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Drops all dead agents from the map.
%% Args: -
%% Returns: {reply, ok, World}.
%%----------------------------------------------------------------------
handle_call({drop, dead}, _From, World=#world{agents=Agents}) ->
  lists:foreach(fun({Pid, _Coordintaes, _Energy}) ->
      gen_server:cast(Pid, dead)
    end, lists:filter(fun({_Pid, _Coordintaes, Energy}) ->
        Energy =< 0
      end, Agents)),
  
  {reply, ok, World}.

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Stops the environment.
%% Args: -
%% Returns: {stop, normal, World}.
%%----------------------------------------------------------------------
%% @doc Interface for the behaviour gen_server.
%%   Only used to stop the environment
handle_cast(stop, World=#world{agents=Agents}) ->
  % send broadcast to all clients
  lists:foreach(fun({Pid, _Coordinates, _Energy}) ->
    gen_server:cast(Pid, world_destroyed)
  end, Agents),
  
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

%%----------------------------------------------------------------------
%% Function: remove_agent/2
%% Purpose: Removes agent from given world and return the new one.
%% Args: Pid as agent pid and World as world.
%% Returns: #world.
%%----------------------------------------------------------------------
%% @doc Removes agent from given world
remove_agent(Pid, World=#world{map=Map, agents=Agents}) ->
  % Free sector on map
  Agent = lists:keyfind(Pid, 1, Agents),
  NewMap = case Agent of
    false ->
      Map;
    _ ->
      {_Pid, Coordinates, _Energy} = Agent,
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
  
  World#world{map=NewMap, agents=NewAgents}.