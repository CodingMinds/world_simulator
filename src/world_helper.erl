%%%---------------------------------------------------------------------
%%% Description module world_helper
%%%---------------------------------------------------------------------
%%% World helper holds some functions which are used by more then one
%%% module.
%%%---------------------------------------------------------------------
%%% Exports
%%% convert_to_map(MapString)
%%%   Translate the given string MapString to a valid map.
%%%---------------------------------------------------------------------

-module(world_helper).
-author('M. Bittorf <info@coding-minds.com>').

-export([convert_to_map/1, send/2, send/3]).

-include("world_records.hrl").

%%----------------------------------------------------------------------
%% Function: convert_to_map/1
%% Purpose: Translate a string in a new map.
%% Args: An ASCII string which represents the new map.
%% Returns: ok | {error, Reason}
%%----------------------------------------------------------------------
convert_to_map(MapString) ->
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
%% Function: send/2
%% Purpose: Format message Str and send it to socket Socket
%% Args: Active socket Socket and message Str
%% Returns: ok
%%----------------------------------------------------------------------
send(Socket, Str) ->
  send(Socket, Str, []).

%%----------------------------------------------------------------------
%% Function: send/3
%% Purpose: Format message Str with arguments Args and send it to socket
%%   Socket
%% Args: Active socket Socket, format arguments Args and message Str
%% Returns: ok
%%----------------------------------------------------------------------
send(Socket, Str, Args) ->
  gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
  inet:setopts(Socket, [{active, once}]),
  ok.
