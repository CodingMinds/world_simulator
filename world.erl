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
  Field = free_field(Map),
  case Field of
    false ->
      {reply, map_full, State};
    {Coordinates, Properties} ->
      
      NewMap = lists:keyreplace(Coordinates, 1, Map, {Coordinates, Properties#field{staffed=true}}),
      
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
  % Free cell on map
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
        {_, Field} ->
          NewField = {Coordinates, Field#field{staffed=false}},
          lists:keyreplace(Coordinates, 1, Map, NewField)
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
  CheckAndApply = fun(MapCell) ->
    case MapCell of
      [] ->
        {reply, {error, blocked}, State};
      _ ->
        {Coordinates, Field} = lists:nth(1, MapCell),
        if
          Field#field.blocked == true ->
            {reply, {error, blocked}, State};
          Field#field.staffed == true ->
            {reply, {error, blocked}, State};
          Field#field.food /= 0 ->
            NewAgents = lists:keyreplace(Pid, 1, Agents, {Pid, Coordinates}),
            {reply, {food, Field#field.food}, State#state{agents=NewAgents}};
          true ->
            NewAgents = lists:keyreplace(Pid, 1, Agents, {Pid, Coordinates}),
            {reply, ok, State#state{agents=NewAgents}}
        end
    end
  end,
    
  case lists:keyfind(Pid, 1, Agents) of
    false ->
      {reply, {error, client_unknown}, State};
    {_Pid, {X, Y}} ->
      case Action of
        {move, Direction} ->
          case Direction of
            1 ->
              MapCell = lists:filter(fun({{Xm, Ym}, _}) -> (X==Xm) and (Y-1==Ym) end, Map),
              CheckAndApply(MapCell);
            2 ->
              MapCell = lists:filter(fun({{Xm, Ym}, _}) -> (X+1==Xm) and (Y-1==Ym) end, Map),
              CheckAndApply(MapCell);
            3 ->
              MapCell = lists:filter(fun({{Xm, Ym}, _}) -> (X+1==Xm) and (Y==Ym) end, Map),
              CheckAndApply(MapCell);
            4 ->
              MapCell = lists:filter(fun({{Xm, Ym}, _}) -> (X+1==Xm) and (Y+1==Ym) end, Map),
              CheckAndApply(MapCell);
            5 ->
              MapCell = lists:filter(fun({{Xm, Ym}, _}) -> (X==Xm) and (Y+1==Ym) end, Map),
              CheckAndApply(MapCell);
            6 ->
              MapCell = lists:filter(fun({{Xm, Ym}, _}) -> (X-1==Xm) and (Y+1==Ym) end, Map),
              CheckAndApply(MapCell);
            7 ->
              MapCell = lists:filter(fun({{Xm, Ym}, _}) -> (X-1==Xm) and (Y==Ym) end, Map),
              CheckAndApply(MapCell);
            8 ->
              MapCell = lists:filter(fun({{Xm, Ym}, _}) -> (X-1==Xm) and (Y-1==Ym) end, Map),
              CheckAndApply(MapCell);
            _ ->
              {reply, {error, bad_arg}, State}
          end;
        environ ->
          {reply, {environ, ["F", "O", "O", "O", ".", "*", "*", "*"]}, State};
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
%% Function: free_field/1
%% Purpose: Return the coordinates of the next free field or false
%% Args: Map as map which should be used.
%% Returns: {{X, Y}, #field} | false.
%%----------------------------------------------------------------------
free_field(Map) ->
  FreeFields = lists:filter(fun({_, #field{staffed=Staffed}}) -> Staffed == false end, Map),
  case FreeFields of
    [] ->
      false;
    _ ->
      [ FreeField | _ ] = FreeFields,
      FreeField
  end.

%%----------------------------------------------------------------------
%% Function: check_and_apply_new_cell/3
%% Purpose: Check if the applied cell is available for the client and
%%   set the new position of the client if necessary
%% Args: MapCell as target location, State as actual state of the
%%   world and Pid as client identifier
%% Returns: {reply, ok, State} | {reply, {food, N}, State} | {reply,
%%   {Error, Reason}, State}
%%----------------------------------------------------------------------
