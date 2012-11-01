%%---------------------------------------------------------------------
%% Data Type: state
%% where:
%%   map: A list of tuples {{x,y}, field} (default is undefined).
%%   agents: A list of tuples {pid, {x,y}} (default is []).
%%----------------------------------------------------------------------
-record(state, {map, agents = []}).

%%---------------------------------------------------------------------
%% Data Type: sector
%% where:
%%   staffed: An atom (default is false).
%%   food: A integer (default is 0).
%%   blocked: An atom (default is false).
%%----------------------------------------------------------------------
-record(sector, {staffed = false, food = 0, blocked = false}).
