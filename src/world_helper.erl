%%%---------------------------------------------------------------------
%%% Description module world_helper
%%%---------------------------------------------------------------------
%%% @author M. Bittorf <info@coding-minds.com>
%%% @copyright 2012 M. Bittorf
%%% @doc {@module} holds some functions which are used by more then one
%%% module of application world.
%%% @end
%%%---------------------------------------------------------------------
%%% Exports
%%% ascii_to_world(PidString)
%%%   Translate a string to a pid and validates if it's a running
%%%   environment.
%%% ascii_to_map(MapString)
%%%   Translate the given string MapString to a valid map.
%%% map_to_ascii(Map)
%%%   Translate a map into as ASCII representation per row.
%%% map_size(Map)
%%%   Extract the highest X and Y positions of the given map.
%%% birth_sector(Map, X, Y)
%%%   Return the coordinates of the birth sector or some error codes.
%%% free_sector(Map)
%%%   Return the coordinates of the next free sector or false.
%%% consume_food(Coordinates, World)
%%%   Will be triggered if some food is consumed and applies the food
%%%   options to the map.
%%% ascii_rep({Coordinates, Sector})
%%%   Return character which represents the world of the given sector.
%%%   Based on Wilson's WOOD1
%%% get_sector(X, Y, Map)
%%%   Get the sector on position X and Y of map Map. Return a blocked
%%%   one if there is no sector on this position.
%%% ascii_to_options(OptionString, Options)
%%%   Translate a string to a new single options record entry.
%%% ascii_to_options(OptionsString)
%%%   Translate a string in a new options record.
%%% options_to_ascii(Options)
%%%   Translate a option record into an list of ASCII representations.
%%% send(Socket, Str)
%%%   Format message Str and send it to socket Socket.
%%% send(Socket, Str, Args)
%%%   Format message Str with arguments Args and send it to socket
%%%   Socket.
%%% send(Target, Str)
%%%   Send message Str with target Target to logging server.
%%% send(Target, Str, Args)
%%%   Format message Str with arguments Args and send it with target
%%%   Target to logging server.
%%%---------------------------------------------------------------------

-module(world_helper).
-author('M. Bittorf <info@coding-minds.com>').

-export([ascii_to_world/1, ascii_to_map/1, map_to_ascii/1, map_size/1,
  birth_sector/3, free_sector/1, consume_food/2, ascii_rep/1,
  get_sector/3, ascii_to_options/2, ascii_to_options/1,
  options_to_ascii/1, send/2, send/3, log/2, log/3]).

-include("world_records.hrl").

%%----------------------------------------------------------------------
%% Function: ascii_to_world/1
%% Purpose: Translate a string to a pid and validates if it's a running
%%   environment.
%% Args: An ASCII string which represents the pid.
%% Returns: Pid | false
%%----------------------------------------------------------------------
%% @doc Translate a string to a pid and validates if it's a running
%%   environment.
ascii_to_world(PidString) ->
  case catch is_process_alive(list_to_pid(PidString)) of
    {'EXIT', _} ->
      false;
    false ->
      false;
    true ->
      Supervisor = whereis(world_envsup),
      Pid = list_to_pid(PidString),
      case process_info(Pid, links) of
        {links, [Supervisor]} ->
          Pid;
        _ ->
          false
      end
  end.

%%----------------------------------------------------------------------
%% Function: ascii_to_map/1
%% Purpose: Translate a string to a new map data structure.
%% Args: An ASCII string which represents the new map.
%% Returns: ok | {error, Reason}
%%----------------------------------------------------------------------
%% @doc Translate a string to a new map data structure.
ascii_to_map(MapString) ->
  %% Split into rows
  Tokens = string:tokens(MapString, "|"),
  
  %% Translate from ASCII to sector records
  Sectors = lists:map(
    fun(Row) ->
      lists:map(
        fun(Char) ->
          case [Char] of
            "F" ->
              #sector{food=1000};
            "O" ->
              #sector{blocked=true};
            _ ->
              #sector{}
          end
        end,
      Row)
    end,
  Tokens),
  
  %% Translate from [[#sector]] into list[{{X,Y}, #sector}]
  {_, Map} = lists:foldl(fun(Row, {Y, RowAcc}) ->
      {_, NewRowAcc} = lists:foldl(fun(Column, {X, ColAcc}) ->
          NewColAcc = ColAcc ++ [{{X, Y}, Column}],
          
          {X + 1, NewColAcc}
        end,
        {1, RowAcc},
        Row),
      
      {Y + 1, NewRowAcc}
    end,
  {1, []},
  Sectors),
  
  Map.

%%----------------------------------------------------------------------
%% Function: map_to_ascii/1
%% Purpose: Translate a map into as ASCII representation per row.
%% Args: The map Map.
%% Returns: [AsciiRow, ..]
%%----------------------------------------------------------------------
%% @doc Translate a map into as ASCII representation per row.
map_to_ascii(Map) ->
  %% Get max size of map
  {X, Y} = world_helper:map_size(Map),
  
  %% create ascii representation based on max size
  lists:foldl(fun(Yi, RowAcc) ->
                 Col = lists:foldl(
                         fun(Xi, ColAcc) ->
                           Sector = ascii_rep(get_sector(Xi, Yi, Map)),
                           case length(Sector) of
                             1 -> ColAcc ++ " " ++ Sector ++ " ";
                             2 -> ColAcc ++ " " ++ Sector;
                             3 -> ColAcc ++ Sector
                           end
                         end,
                       [], lists:seq(1, X)),
                 RowAcc ++ [Col]
               end,
             [], lists:seq(1, Y)).

%%----------------------------------------------------------------------
%% Function: map_size/1
%% Purpose: Extract the highest X and Y positions of the given map.
%% Args: map Map.
%% Returns: {X, Y}
%%----------------------------------------------------------------------
%% @doc Extract the highest X and Y positions of the given map.
map_size([]) ->
  {1, 1};

map_size(Map) ->
  Coordinates = lists:map(fun({{X, Y}, _}) -> {X, Y} end, Map),
  lists:nth(1,
    lists:sort(fun({Xa, Ya}, {Xb, Yb}) ->
                 (Xb < Xa) and (Yb < Ya)
               end,
               Coordinates)).

%%----------------------------------------------------------------------
%% Function: birth_sector/3
%% Purpose: Return the coordinates of the birth sector or some error
%%   codes.
%% Args: World as map which should be used, X and Y as desired place. If
%%   X or Y are 0 they will be ignored.
%% Returns: {{X, Y}, #sector} | {error, map_full}
%%   | {error, access_denied} | {error, invalid_position}.
%%----------------------------------------------------------------------
%% @doc Return the coordinates of the birth sector or some error codes.
birth_sector(#world{options=Options, map=Map}, X, Y) ->
  if
    (X == 0) or (Y == 0) ->
      free_sector(Map);
    Options#options.allow_startposition == false ->
      {error, access_denied};
    true ->
      FreeSectors = lists:filter(
        fun({{Xi, Yi}, #sector{staffed=Staffed, blocked=Blocked}}) ->
          (Xi == X) and (Yi == Y) and
          (Staffed == 0) and (Blocked == false)
        end, Map),
        
      case FreeSectors of
        [] ->
          {error, invalid_position};
        _ ->
          hd(FreeSectors)
      end
  end.

%%----------------------------------------------------------------------
%% Function: free_sector/1
%% Purpose: Return the coordinates of the next free sector or map_full.
%% Args: Map as map which should be used.
%% Returns: {{X, Y}, #sector} | {error, map_full}.
%%----------------------------------------------------------------------
%% @doc Return the coordinates of the next free sector or
%%   {error, map_full}.
free_sector(Map) ->
  FreeSectors = lists:filter(
    fun({_, #sector{staffed=Staffed, blocked=Blocked}}) ->
      (Staffed == 0) and (Blocked == false)
    end, Map),
  
  case FreeSectors of
    [] ->
      {error, map_full};
    _ ->
      hd(FreeSectors)
  end.

%%----------------------------------------------------------------------
%% Function: consume_food/2
%% Purpose: Will be triggered if some food is consumed and applies the
%%   food options to the map.
%% Args: {X, Y} coordinates of food, World as world.
%% Returns: The modified world.
%%----------------------------------------------------------------------
%% @doc Will be triggered if some food is consumed and applies the food
%%   options to the map.
consume_food({X, Y}, World=#world{map=Map, options=Options,
  agents=_Agents}) ->
  {Coordinates, Sector} = world_helper:get_sector(X, Y, Map),
  case Options#options.respawn_food of
    true ->
      case Options#options.static_food of
        true -> % nothing happens
          World;
        _ -> % try to move food away
          % search possible new locations
          PosTargets = lists:filter(fun({_ICoordinates, ISector}) ->
                         (ISector#sector.blocked /= true) and
                         (ISector#sector.food == 0)
                       end, Map),
          case PosTargets of
            [] -> % no possible movements
              World;
            Targets -> % move
              random:seed(now()),
              
              {Coordinates2, Sector2} =
                lists:nth(random:uniform(length(Targets)), Targets),
              
              Map2 = lists:keyreplace(Coordinates, 1, Map,
                {Coordinates, Sector#sector{food=0}}),
              
              Map3 = lists:keyreplace(Coordinates2, 1, Map2,
               {Coordinates2, Sector2#sector{food=Sector#sector.food}}),
              
              log(env, "moved food from ~w to ~w",
                [Coordinates, Coordinates2]),
              
              % send broadcast to all clients
              % deactivated because it's deprecated
              %lists:foreach(fun({Pid, _Coordinates, _Fitness}) ->
              %  gen_server:cast(Pid, world_changed)
              %end, Agents),
              
              World#world{map=Map3}
          end
      end;
    _ -> % delete food from map
      NewMap = lists:keyreplace({X, Y}, 1, Map,
        {{X, Y}, Sector#sector{food=0}}),
      World#world{map=NewMap}
  end.

%%----------------------------------------------------------------------
%% Function: ascii_rep/1
%% Purpose: Return character which represents the world of the given
%%   sector. Based on Wilson's WOOD1.
%% Args: Sector.
%% Returns: . | O  | F | *
%%----------------------------------------------------------------------
%% @doc Return character which represents the world of the given sector.
%%   Based on Wilson's WOOD1.
ascii_rep({_Coordinates, Sector}) ->
  if
    Sector#sector.blocked == true ->
      "O";
    Sector#sector.staffed /= 0 ->
      "*" ++ integer_to_list(Sector#sector.staffed);
    Sector#sector.food /= 0 ->
      "F";
    true ->
      "."
  end.

%%----------------------------------------------------------------------
%% Function: get_sector/3
%% Purpose: Get the sector on position X and Y of map Map. Return a
%%  blocked one if there is no sector on this position.
%% Args: X and Y as coordinates on map Map.
%% Returns: {{X, Y}, #sector}
%%----------------------------------------------------------------------
%% @doc Get the sector on position X and Y of map `Map'. Return a
%%   blocked one if there is no sector on this position.
get_sector(X, Y, Map) ->
  case lists:filter(fun({{Xm, Ym}, _}) ->
                    (X==Xm) and (Y==Ym) end, Map) of
    [] ->
      {{X, Y}, #sector{blocked=true}};
    SectorList ->
      lists:nth(1, SectorList)
  end.

%%----------------------------------------------------------------------
%% Function: ascii_to_options/2
%% Purpose: Translate a string to a new single options record entry.
%% Args: An ASCII string which represents the new option.
%% Returns: {ok, #options} | {error, Reason}
%%----------------------------------------------------------------------
%% @doc Translate a string to a new single options record entry.
ascii_to_options(OptionString, Options) ->
  CleanOptionString = hd(string:tokens(OptionString, "\r\n")),
  Option = string:tokens(CleanOptionString, " "),
  try
    case length(Option) of
      2 ->
        case hd(Option) of
          "max_agents" ->
            Opt = list_to_integer(lists:nth(2, Option)),
            if
              Opt < 0 ->
                {error, bad_argument};
              true ->
                {ok, Options#options{max_agents=Opt}}
            end;
          "respawn_food" ->
            Opt = list_to_atom(lists:nth(2, Option)),
            if
              (Opt /= true) and (Opt /= false) ->
                {error, bad_argument};
              true ->
                {ok, Options#options{respawn_food=Opt}}
            end;
          "static_food" ->
            Opt = list_to_atom(lists:nth(2, Option)),
            if
              (Opt /= true) and (Opt /= false) ->
                {error, bad_argument};
              true ->
                {ok, Options#options{static_food=Opt}}
            end;
          "env_name" ->
            Opt = list_to_atom(lists:nth(2, Option)),
              {ok, Options#options{env_name=Opt}};
          "allow_startposition" ->
            Opt = list_to_atom(lists:nth(2, Option)),
            if
              (Opt /= true) and (Opt /= false) ->
                {error, bad_argument};
              true ->
                {ok, Options#options{allow_startposition=Opt}}
            end;
          "initial_fitness" ->
            Opt = list_to_atom(lists:nth(2, Option)),
            if
              Opt < 0 ->
                {error, bad_argument};
              true ->
                {ok, Options#options{initial_fitness=Opt}}
            end;
          "fitness_nomove" ->
            Opt = list_to_integer(lists:nth(2, Option)),
            {ok, Options#options{fitness_nomove=Opt}};
          "fitness_blocked" ->
            Opt = list_to_integer(lists:nth(2, Option)),
            {ok, Options#options{fitness_blocked=Opt}};
          "fitness_staffed" ->
            Opt = list_to_integer(lists:nth(2, Option)),
            {ok, Options#options{fitness_staffed=Opt}};
          "fitness_moved" ->
            Opt = list_to_integer(lists:nth(2, Option)),
            {ok, Options#options{fitness_moved=Opt}};
          "drop_agents" ->
            Opt = list_to_atom(lists:nth(2, Option)),
            if
              (Opt /= true) and (Opt /= false) ->
                {error, bad_argument};
              true ->
                {ok, Options#options{drop_agents=Opt}}
            end;
          _ ->
            {error, bad_argument}
        end;
      _ ->
        {error, bad_argument_count}
    end
  catch
    Exception:Reason -> {error, {exception, Exception, Reason}}
  end.

%%----------------------------------------------------------------------
%% Function: ascii_to_options/1
%% Purpose: Translate a string to a new options record.
%% Args: An ASCII string which represents the new options.
%% Returns: {ok, #options} | {error, Reason}
%%----------------------------------------------------------------------
%% @doc Translate a string to a new options record.
ascii_to_options(OptionsString) ->
  Result = (catch case string:tokens(OptionsString, " ") of
    [MaxAgents, RespawnFood, StaticFood, EName, AStartPosition,
     InitialFitness, FitnessNotMoved, FitnessBlocked, FitnessStaffed,
     FitnessMoved, DropAgents] ->
      MAgents = list_to_integer(MaxAgents),
      RFood = list_to_atom(RespawnFood),
      SFood = list_to_atom(StaticFood),
      AStartPos = list_to_atom(AStartPosition),
      IFitness = list_to_integer(InitialFitness),
      FNMoved = list_to_integer(FitnessNotMoved),
      FBlocked = list_to_integer(FitnessBlocked),
      FStaffed = list_to_integer(FitnessStaffed),
      FMoved = list_to_integer(FitnessMoved),
      DAgents = list_to_atom(DropAgents),
      
      % ugly way to verify that the atoms are true or false
      % and the integer >= 0
      if
        MAgents < 0 ->
          {error, bad_arg};
        IFitness < 0 ->
          {error, bad_arg};
        RFood or SFood or AStartPos or DAgents or true ->
          #options{
            max_agents = MAgents,
            respawn_food = RFood,
            static_food = SFood,
            env_name = EName,
            allow_startposition = AStartPos,
            initial_fitness = IFitness,
            fitness_nomove = FNMoved,
            fitness_blocked = FBlocked,
            fitness_staffed = FStaffed,
            fitness_moved = FMoved,
            drop_agents = DAgents
          };
        true ->
          {error, bad_arg}
      end;
    _ ->
      {error, bad_arg}
  end),
  
  case Result of
    {'EXIT', _Reason} ->
      {error, bad_arg};
    {error, _Reason} ->
      {error, bad_arg};
    R ->
      {ok, R}
  end.

%%----------------------------------------------------------------------
%% Function: options_to_ascii/1
%% Purpose: Translate a option record into a list of ASCII
%%   representations.
%% Args: The option Option.
%% Returns: String.
%%----------------------------------------------------------------------
%% @doc Translate a option record into a list of ASCII representations.
options_to_ascii(Options) when is_record(Options, options) ->
  [
  "max agents (max_agents): " ++
    integer_to_list(Options#options.max_agents),
  "respawn food (respawn_food): " ++
    atom_to_list(Options#options.respawn_food),
  "static food positions (static_food): " ++
    atom_to_list(Options#options.static_food),
  "environment name (env_name): " ++
    atom_to_list(Options#options.env_name),
  "allow start position (allow_startposition): " ++
    atom_to_list(Options#options.allow_startposition),
  "initial fitness (initial_fitness): " ++
    integer_to_list(Options#options.initial_fitness),
  "fitness reduction if agent not moved (fitness_nomove): " ++
    integer_to_list(Options#options.fitness_nomove),
  "fitness reduction if section blocked (fitness_blocked): " ++
    integer_to_list(Options#options.fitness_blocked),
  "fitness reduction if section staffed (fitness_staffed): " ++
    integer_to_list(Options#options.fitness_staffed),
  "fitness reduction if agent moved (fitness_moved): " ++
    integer_to_list(Options#options.fitness_moved),
  "drop agents if their fitness reaches 0 (drop_agents): " ++
    atom_to_list(Options#options.drop_agents)
  ].

%%----------------------------------------------------------------------
%% Function: send/2
%% Purpose: Send message Str to socket Socket.
%% Args: Active socket Socket and message Str
%% Returns: ok
%%----------------------------------------------------------------------
%% @doc Send message `Str' to socket `Socket'.
send(Socket, Str) ->
  send(Socket, Str, []).

%%----------------------------------------------------------------------
%% Function: send/3
%% Purpose: Format message Str with arguments Args and send it to socket
%%   Socket.
%% Args: Active socket Socket, format arguments Args and message Str
%% Returns: ok
%%----------------------------------------------------------------------
%% @doc Format message `Str' with arguments `Args' and send it to socket
%%   `Socket'.
send(Socket, Str, Args) ->
  gen_tcp:send(Socket, io_lib:format(Str ++ "~n", Args)),
  inet:setopts(Socket, [{active, once}]),
  ok.

%%----------------------------------------------------------------------
%% Function: log/2
%% Purpose: Send message Str with target Target to logging server.
%% Args: Message Str and target Target.
%% Returns: ok
%%----------------------------------------------------------------------
%% @doc Send message `Str' with target `Target' to logging server.
log(Target, Str) ->
  log(Target, Str, []).

%%----------------------------------------------------------------------
%% Function: log/3
%% Purpose: Format message Str with arguments Args and send it with
%%   target Target to logging server.
%% Args: Message Str, format arguments Arg and target Target.
%% Returns: ok
%%----------------------------------------------------------------------
%% @doc Format message `Str' with arguments `Args' and send it with
%%   target `Target' to logging server.
log(Target, Str, Args) ->
  gen_server:cast(world_logging, {log, Target,
    io_lib:format(Str ++ "~n", Args)}).