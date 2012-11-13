%%---------------------------------------------------------------------
%% Data Type: options
%% where:
%%   max_agents: A integer (default is 0).
%%   respawn_food: An atom (default is true).
%%   static_food: An atom (default is true).
%%   env_name: A string (default is "").
%%   allow_startposition: An atom (default is true).
%%----------------------------------------------------------------------
-record(options, {max_agents = 0, respawn_food = true,
  static_food = true, env_name = "unknown",
  allow_startposition = true}).

%%---------------------------------------------------------------------
%% Data Type: world
%% where:
%%   map: A list of tuples {{x,y}, sector} (default is []).
%%   options: A options record.
%%   agents: A list of tuples {pid, {x,y}} (default is []).
%%----------------------------------------------------------------------
-record(world, {map = [], options = #options{}, agents = []}).

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
%%   environ: A Pid of world_env (default is undefined)
%%----------------------------------------------------------------------
-record(sstate, {socket, environ}).
