-module(coderlx_mcp).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        server_auth_login/2
      , server_status_list/2
    ]).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, mcp_server_oauth_login_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(mcp_server_oauth_login_response)
      , {alias, {coderlx, coderlx}}
    ]}).
server_auth_login(Params, Coderlx) ->
    coderlx:request('mcpServer/oauth/login', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, mcp_server_status_list_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(mcp_server_status_list_response)
      , {alias, {coderlx, coderlx}}
    ]}).
server_status_list(Params, Coderlx) ->
    coderlx:request('mcpServerStatus/list', Params, Coderlx).
