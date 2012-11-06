%%---------------------------------------------------------------------
%% Data Type: options
%% where:
%%   static_food: An atom (default is true).
%%   respawn_food: An atom (default is true).
%%   respawn_time: A integer (default is 0).
%%   max_agents: A integer (default is 0).
%%----------------------------------------------------------------------
-record(options, {max_agents = 0, respawn_food = true,
  static_food = true, respawn_time = 0}).

%%---------------------------------------------------------------------
%% Data Type: world
%% where:
%%   map: A list of tuples {{x,y}, sector} (default is undefined).
%%   options: A options record.
%%   agents: A list of tuples {pid, {x,y}} (default is []).
%%----------------------------------------------------------------------
-record(world, {map, options = #options{}, agents = []}).

%%---------------------------------------------------------------------
%% Data Type: sector
%% where:
%%   staffed: An atom (default is false).
%%   food: A integer (default is 0).
%%   blocked: An atom (default is false).
%%----------------------------------------------------------------------
-record(sector, {staffed = false, food = 0, blocked = false}).

%%---------------------------------------------------------------------
%% Data Type: sstate
%% where:
%%   socket: A socket (default is undefined).
%%----------------------------------------------------------------------
-record(sstate, {socket}).
