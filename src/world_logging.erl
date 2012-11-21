%%%---------------------------------------------------------------------
%%% Description module world_logging
%%%---------------------------------------------------------------------
%%% @author M. Bittorf <info@coding-minds.com>
%%% @copyright 2012 M. Bittorf
%%% @doc {@module} get log messages as casts from the different modules,
%%% sort them and write it down.
%%% @end
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
%%% handle_cast({log, error, Msg}, State)
%%%   Interface for the behaviour gen_server.
%%%   Handles incoming log messages with environment informations.
%%% handle_cast({log, info, Msg}, State)
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
%% Function: start_link/0
%% Purpose: Wrapper for start_link of gen_server
%% Args: -
%% Returns: 
%%----------------------------------------------------------------------
%% @doc Wrapper for start_link of gen_server.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose: Interface for the behaviour gen_server.
%% Args: Argument is ignored
%% Returns: {ok, {ClientLog, EnvLog}}
%%----------------------------------------------------------------------
%% @doc Interface for the behaviour gen_server.
init(_Argument) ->
  {ok, ClientLog} = application:get_env(log_client),
  {ok, EnvLog} = application:get_env(log_env),
  {ok, ErrorLog} = application:get_env(log_error),
  {ok, InfoLog} = application:get_env(log_info),
  
  {ok, {ClientLog, EnvLog, ErrorLog, InfoLog}}.

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Handles incoming log messages with client informations and
%%   write them down to the configured file.
%% Args: -
%% Returns: {noreply, State}
%%----------------------------------------------------------------------
%% @doc Interface for the behaviour gen_server.
%%   Handles incoming log messages.
handle_cast({log, client, Msg}, State = {ClientLog, _E, _E, _I}) ->
  ok = log(ClientLog, Msg),
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Handles incoming log messages with client informations and
%%   write them down to the configured file.
%% Args: -
%% Returns: {noreply, State}
%%----------------------------------------------------------------------
handle_cast({log, env, Msg}, State = {_C, EnvLog, _E, _I}) ->
  ok = log(EnvLog, Msg),
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Handles incoming error messages with error informations and
%%   write them down to the configured file.
%% Args: -
%% Returns: {noreply, State}
%%----------------------------------------------------------------------
handle_cast({log, error, Msg}, State = {_C, _E, ErrorLog, _I}) ->
  ok = log(ErrorLog, Msg),
  {noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Handles incoming log messages with info informations and
%%   write them down to the configured file.
%% Args: -
%% Returns: {noreply, State}
%%----------------------------------------------------------------------
handle_cast({log, info, Msg}, State = {_C, _E, _E, InfoLog}) ->
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
%% @doc Dummy function for the behaviour gen_server. Not used in this
%% implementation
handle_call(_Action, _From, State) ->{noreply, State}.
%% @doc Dummy function for the behaviour gen_server. Not used in this
%% implementation
handle_info(_Message, State) -> {noreply, State}.
%% @doc Dummy function for the behaviour gen_server. Not used in this
%% implementation
terminate(normal, _State) -> ok;
terminate(_Reason, _State) -> ok.
%% @doc Dummy function for the behaviour gen_server. Not used in this
%% implementation
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%----------------------------------------------------------------------
%% Function: log/2
%% Purpose: Write message Msg with timestamp to file Target.
%% Args: Target, Msg
%% Returns: ok | {error, Reason}
%%----------------------------------------------------------------------
%% @doc Write message `Msg' with timestamp to file `Target'.
%% @private
log(Target, Msg) ->
  {Y, Mo, D} = date(),
  {H, Mi, S} = time(),
  LogMsg = io_lib:format("~b-~b-~b ~b:~b:~b ", [Y, Mo, D, H, Mi, S]) ++
    Msg,
  file:write_file(Target, LogMsg, [append]).
