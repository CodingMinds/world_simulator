%%%---------------------------------------------------------------------
%%% Description module world_ofo_sup
%%%---------------------------------------------------------------------
%%% @author M. Bittorf <info@coding-minds.com>
%%% @copyright 2012 M. Bittorf
%%% @doc The one for one supervisor {@module} which monitors all
%%% supervisors and workers that can restarted without big trouble or
%%% dependencies.
%%% @end
%%%---------------------------------------------------------------------
%%% Exports
%%% start_link()
%%%   Interface for the behaviour supervisor.
%%%   Starts the supervisor.
%%% init([])
%%%   Interface for the behaviour supervisor.
%%%   Returns the child specification for the supervisor.
%%%---------------------------------------------------------------------

-module(world_ofo_sup).
-author('M. Bittorf <info@coding-minds.com>').

-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("world_records.hrl").

%%----------------------------------------------------------------------
%% Function: start_link/0
%% Purpose: Interface for the behaviour supervisor.
%%   Starts the supervisor.
%% Args: -
%% Returns: {ok, Pid} | ignore | {error, Reason}
%%----------------------------------------------------------------------
%% @doc Wrapper for start_link of supervisor.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose: Interface for the behaviour supervisor.
%%   Returns the child specification for the supervisor.
%% Args: -
%% Returns: child_spec()
%%----------------------------------------------------------------------
%% @doc Interface for the behaviour supervisor.
%%   Returns the child specification for the supervisor.
init([]) ->
  MaxRestart = 6,
  MaxTime = 3000,
  
  {ok,
   {{one_for_one, MaxRestart, MaxTime},
   [
    {logging,
     {world_logging, start_link, []},
     permanent, 500, worker, [world_logging]},
    {ctl_sservsup,
     {world_ctl_sservsup, start_link, []},
     permanent, infinity, supervisor, [world_ctl_sservsup]},
    {sservsup,
     {world_sservsup, start_link, []},
     permanent, infinity, supervisor, [world_sservsup]},
    {http,
     {world_http, start_link, []},
     permanent, 500, worker, [world_http]}
   ]}}.
