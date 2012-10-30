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
%% Purpose: Add the agent behind pid From to the map.
%% Args: -
%% Returns: {reply, ok, #state} | {reply, map_full, #state}.
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
%% Purpose: 
%% Args: -
%% Returns: {reply, ok, #state} | {reply, {food, Amount}, #state} |
%%   {reply, {error, Reason}, #state}.
%%----------------------------------------------------------------------
handle_call({do, _Action}, {Pid, _Tag}, State=#state{map=_Map, agents=Agents}) ->
  case lists:keyfind(Pid, 1, Agents) of
    false ->
      {reply, {error, client_unknown}, State};
    _ ->
      % TODO: Handle actions
      {reply, ok, State}
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
