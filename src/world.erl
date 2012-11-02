%%%---------------------------------------------------------------------
%%% Description module world
%%%---------------------------------------------------------------------
%%% World is the application which combines the simulated environment,
%%% the socket server which handles communications and the supervisors.
%%%---------------------------------------------------------------------
%%% Exports
%%% start(normal, Args)
%%%   Starts the application.
%%% stop(State)
%%%   Stops the application.
%%%---------------------------------------------------------------------

-module(world).
-author('M. Bittorf <info@coding-minds.com>').

-behaviour(application).

-export([start/2, stop/1]).

%%----------------------------------------------------------------------
%% Function: start/1
%% Purpose: Starts the application
%% Args: -
%% Returns: ok | {Error, Reason}
%%----------------------------------------------------------------------
start(normal, _Args) ->
  world_sup:start_link().

%%----------------------------------------------------------------------
%% Function: stop/1
%% Purpose: Stops the application
%% Args: -
%% Returns: ok.
%%----------------------------------------------------------------------
stop(_State) ->
  ok.
