%%%---------------------------------------------------------------------
%%% Description module world_http
%%%---------------------------------------------------------------------
%%% Http uses the erlang intes http implementation to provide a read
%%% only interface to the environment.
%%%---------------------------------------------------------------------
%%% Exports
%%% start_link()
%%%  Interface for the calling behaviour supervisor.
%%%  Starts inets and the inets http daemon.
%%% map(SessionID, Env, Input)
%%%   Interface for mod_esi to serve an ASCII map.
%%% options(SessionID, Env, Input)
%%%   Interface for mod_esi to serve ASCII options.
%%%---------------------------------------------------------------------

-module(world_http).
-author('M. Bittorf <info@coding-minds.com>').

-export([start_link/0, map/3, options/3]).

-include("world_records.hrl").

%%----------------------------------------------------------------------
%% Function: start_link/0
%% Purpose: Interface for the calling behaviour supervisor.
%%  Starts inets http daemon.
%% Args: -
%% Returns: {ok, Pid}
%%----------------------------------------------------------------------
start_link() ->
  {ok, HttpPort} = application:get_env(http_port),
  {ok, Log} = application:get_env(log_http),
  {ok, LogError} = application:get_env(log_http_error),
  
  inets:start(httpd, [
    {modules, [
      mod_alias,
      mod_auth,
      mod_esi,
      mod_actions,
      mod_cgi,
      mod_dir,
      mod_get,
      mod_head,
      mod_log,
      mod_disk_log]},
    {port, HttpPort},
    {server_name, "world_simulator"},
    {server_root, "."},
    {document_root, "../htdocs"},
    {erl_script_alias, {"/erl", [world_http]}},
    {error_log, LogError},
    {security_log, Log},
    {transfer_log, Log},
    {mime_types, [
      {"html","text/html"},
      {"css","text/css"},
      {"js","application/x-javascript"}
    ]}
  ], stand_alone).

%%----------------------------------------------------------------------
%% Function: map/3
%% Purpose: Interface for mod_esi to server an ASCII map.
%% Args: SessionId, Environment and Input from httpd
%% Returns: mod_esi:deliver return value.
%%----------------------------------------------------------------------
map(SessionID, _Env, _Input) ->
  Content = case gen_server:call(world_env, map) of
    {map, Map} ->
      AsciiRows = world_helper:map_to_ascii(Map),
      string:join(AsciiRows, "\n");
    {error, Reason} ->
      io_lib:format("Error: ~p", [Reason])
  end,
  
  mod_esi:deliver(SessionID, [
    "Content-Type: text/plain\r\n\r\n",
    Content
  ]).

%%----------------------------------------------------------------------
%% Function: map/3
%% Purpose: Interface for mod_esi to serve ASCII options.
%% Args: SessionId, Environment and Input from httpd
%% Returns: mod_esi:deliver return value.
%%----------------------------------------------------------------------
options(SessionID, _Env, _Input) ->
  Content = case gen_server:call(world_env, options) of
    {options, Options} ->
      AsciiOptions = world_helper:options_to_ascii(Options),
      string:join(AsciiOptions, "\n");
    {error, Reason} ->
      io_lib:format("Error: ~p", [Reason])
  end,
  
  mod_esi:deliver(SessionID, [
    "Content-Type: text/plain\r\n\r\n",
    Content
  ]).