%%---------------------------------------------------------------------
%% Data Type: options
%% where:
%%   max_agents: An integer (default is 0).
%%   respawn_food: An atom (default is true).
%%   static_food: An atom (default is true).
%%   env_name: An atom (default is unknown).
%%   allow_startposition: An atom (default is true).
%%   fitness_nomove: An integer (default is 0).
%%   fitness_blocked: An integer (default is 0).
%%   fitness_staffed: An integer (default is 0).
%%   fitness_moved: An integer (default is 0).
%%   initial_energy: An integer (default is 1000).
%%   energy_nomove: An integer (default is 1).
%%   energy_blocked: An integer (default is 3).
%%   energy_staffed: An integer (default is 3).
%%   energy_moved: An integer (default is 2).
%%   drop_agents: An atom (default: false).
%%   time_slice: An integer (default: 0).
%%----------------------------------------------------------------------
-record(options, {max_agents = 0, respawn_food = true,
  static_food = true, env_name = unknown,
  allow_startposition = true, fitness_nomove = 0, fitness_blocked = 0,
  fitness_staffed = 0, fitness_moved = 0, initial_energy = 1000,
  energy_nomove = 1, energy_blocked = 3, energy_staffed = 3,
  energy_moved = 2, drop_agents = false, time_slice = 0}).

%%---------------------------------------------------------------------
%% Data Type: world
%% where:
%%   map: A list of tuples {{x,y}, sector} (default is []).
%%   options: An options record.
%%   agents: A list of tuples {pid, {x,y}, fitness} (default is []).
%%----------------------------------------------------------------------
-record(world, {map = [], options = #options{}, agents = []}).

%%---------------------------------------------------------------------
%% Data Type: sector
%% where:
%%   staffed: An integer (default is 0).
%%   food: An integer (default is 0).
%%   blocked: An atom (default is false).
%%----------------------------------------------------------------------
-record(sector, {staffed = 0, food = 0, blocked = false}).

%%---------------------------------------------------------------------
%% Data Type: sstate
%% where:
%%   socket: A socket (default is undefined).
%%   environ: A Pid of world_env (default is undefined)
%%----------------------------------------------------------------------
-record(sstate, {socket, environ}).
