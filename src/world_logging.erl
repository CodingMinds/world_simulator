%%%---------------------------------------------------------------------
%%% Description module world_logging
%%%---------------------------------------------------------------------
%%% Logging get log messages as casts from the different modules, sort
%%% them and write it down.
%%%---------------------------------------------------------------------
%%% Exports
%%% init([])
%%%   Interface for the behaviour gen_server.
%%% handle_cast({log, client, Msg}, State)
%%%   Interface for the behaviour gen_server.
%%%   Handles incoming log messages with client informations.
%%% handle_cast({log, env, Msg}, State)
%%%   Interface for the behaviour gen_server.
%%%   Handles incoming log messages with environment informations.
%%% handle_cast(Event, State)
%%%   Handles unkown messages to flush event queue.
%%%---------------------------------------------------------------------

-module(world_logging).
-author('M. Bittorf <info@coding-minds.com>').

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-include("world_records.hrl").

%%----------------------------------------------------------------------
%% Function: start/0
%% Purpose: Wraper for start_link of gen_server
%% Args: -
%% Returns: 
%%----------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose: Interface for the behaviour gen_server.
%% Args: Argument is ignored
%% Returns: {ok, {ClientLog, EnvLog}}
%%----------------------------------------------------------------------
init(_Argument) ->
  {ok, ClientLog} = application:get_env(log_client),
  {ok, EnvLog} = application:get_env(log_env),
  {ok, InfoLog} = application:get_env(log_info),
  
  {ok, {ClientLog, EnvLog, InfoLog}}.

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Handles incoming log messages with client informations and
%%   write them down to the configured file.
%% Args: -
%% Returns: {noreply, State}
%%----------------------------------------------------------------------
handle_cast({log, client, Msg}, State = {ClientLog, _Env, _Info}) ->
  ok = log(ClientLog, Msg),
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Handles incoming log messages with client informations and
%%   write them down to the configured file.
%% Args: -
%% Returns: {noreply, State}
%%----------------------------------------------------------------------
handle_cast({log, env, Msg}, State = {_Client, EnvLog, _Info}) ->
  ok = log(EnvLog, Msg),
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Handles incoming log messages with info informations and
%%   write them down to the configured file.
%% Args: -
%% Returns: {noreply, State}
%%----------------------------------------------------------------------
handle_cast({log, info, Msg}, State = {_Client, _Env, InfoLog}) ->
  ok = log(InfoLog, Msg),
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Handles unknown messages to flush event queue.
%% Args: -
%% Returns: {noreply, State}
%%----------------------------------------------------------------------
handle_cast(_Event, State) ->
  {noreply, State}.

%%----------------------------------------------------------------------
%% Function: *
%% Purpose: Dummy functions for the behaviour gen_server
%%----------------------------------------------------------------------
handle_call(_Action, _From, State) ->{noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(normal, _State) -> ok;
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%----------------------------------------------------------------------
%% Function: log/2
%% Purpose: Write message Msg with timestamp to for Target configured
%%   file.
%% Args: Target, Msg
%% Returns: ok | {error, Reason}
%%----------------------------------------------------------------------
log(Target, Msg) ->
  {Y, Mo, D} = date(),
  {H, Mi, S} = time(),
  LogMsg = io_lib:format("~b-~b-~b ~b:~b:~b ", [Y, Mo, D, H, Mi, S]) ++
    Msg,
  file:write_file(Target, LogMsg, [append]).
