%%%---------------------------------------------------------------------
%%% Description module world_http
%%%---------------------------------------------------------------------
%%% @author M. Bittorf <info@coding-minds.com>
%%% @copyright 2012 M. Bittorf
%%% @doc {@module} uses the erlang inets http implementation to provide
%%% a read only interface to the environment.
%%% @end
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

-export([start_link/0, worlds/3, map/3, options/3]).

-include("world_records.hrl").

%%----------------------------------------------------------------------
%% Function: start_link/0
%% Purpose: Interface for the calling behaviour supervisor. Starts inets
%%   http daemon.
%% Args: -
%% Returns: {ok, Pid}
%%----------------------------------------------------------------------
%% @doc Interface for the calling behaviour supervisor. Starts inets
%%   http daemon.
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
%% Function: worlds/3
%% Purpose: Interface for mod_esi to server a list of all worlds.
%% Args: SessionId, Environment and Input from httpd
%% Returns: mod_esi:deliver return value.
%%----------------------------------------------------------------------
%% @doc Interface for mod_esi to server an ASCII map.
worlds(SessionID, _Env, _Input) ->
  Worlds = [{Pid, gen_server:call(Pid, info)} || {_,Pid,_,_}
    <- supervisor:which_children(world_envsup)],
  
  AsciiWorlds = lists:map(
    fun({Pid, {info, Name, X, Y, Agents, MAgents}}) ->
      io_lib:format("{"
        "\"Pid\": \"~w\","
        "\"Name\": \"~s\","
        "\"X\": ~B,"
        "\"Y\": ~B,"
        "\"AgentCount\": ~B,"
        "\"MaxAgents\": ~B"
        "}",
        [Pid, Name, X, Y, Agents, MAgents])
    end, Worlds),
  
  Content = "[ " ++ string:join(AsciiWorlds, ", ") ++ " ]",
  
  io:format("~s~n", [Content]),
  
  mod_esi:deliver(SessionID, [
    "Content-Type: application/json\r\n\r\n",
    Content
  ]).

%%----------------------------------------------------------------------
%% Function: map/3
%% Purpose: Interface for mod_esi to server an ASCII map.
%% Args: SessionId, Environment and Input from httpd
%% Returns: mod_esi:deliver return value.
%%----------------------------------------------------------------------
%% @doc Interface for mod_esi to server an ASCII map.
map(SessionID, _Env, Input) ->
  DecodedInput = http_uri:decode(Input),
  Content = case world_helper:ascii_to_world(DecodedInput) of
    false ->
      io_lib:format("Error: Wrong or no parameter (~s). Use ?<Pid>",
        [DecodedInput]);
    Pid ->
      case gen_server:call(Pid, map) of
        {map, Map} ->
          AsciiRows = world_helper:map_to_ascii(Map),
          string:join(AsciiRows, "\n");
        {error, Reason} ->
          io_lib:format("Error: ~p", [Reason])
      end
  end,
  
  mod_esi:deliver(SessionID, [
    "Content-Type: text/plain\r\n\r\n",
    Content
  ]).

%%----------------------------------------------------------------------
%% Function: options/3
%% Purpose: Interface for mod_esi to serve ASCII options.
%% Args: SessionId, Environment and Input from httpd
%% Returns: mod_esi:deliver return value.
%%----------------------------------------------------------------------
%% @doc Interface for mod_esi to serve ASCII options.
options(SessionID, _Env, Input) ->
  DecodedInput = http_uri:decode(Input),
  Content = case world_helper:ascii_to_world(DecodedInput) of
    false ->
      io_lib:format("Error: Wrong or no parameter (~s). Use ?<Pid>",
        [DecodedInput]);
    Pid ->
      case gen_server:call(Pid, options) of
        {options, Options} ->
          AsciiOptions = world_helper:options_to_ascii(Options),
          string:join(AsciiOptions, "\n");
        {error, Reason} ->
          io_lib:format("Error: ~p", [Reason])
      end
  end,
  
  mod_esi:deliver(SessionID, [
    "Content-Type: text/plain\r\n\r\n",
    Content
  ]).