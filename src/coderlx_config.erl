-module(coderlx_config).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        read/2
      , value_write/2
      , batch_write/2
      , requirements_read/2
      , mcp_server_reload/2
    ]).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, config_read_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(config_read_response)
      , {alias, {coderlx, coderlx}}
    ]}).
read(Params, Coderlx) ->
    coderlx:request('config/read', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, config_value_write_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(config_write_response)
      , {alias, {coderlx, coderlx}}
    ]}).
value_write(Params, Coderlx) ->
    coderlx:request('config/value/write', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, config_batch_write_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(config_write_response)
      , {alias, {coderlx, coderlx}}
    ]}).
batch_write(Params, Coderlx) ->
    coderlx:request('config/batchWrite', Params, Coderlx).

-klsn_input_rule([
        {exact, null}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(config_requirements_read_response)
      , {alias, {coderlx, coderlx}}
    ]}).
requirements_read(Params, Coderlx) ->
    coderlx:request('configRequirements/read', Params, Coderlx).

-klsn_input_rule([
        {exact, null}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(mcp_server_refresh_response)
      , {alias, {coderlx, coderlx}}
    ]}).
mcp_server_reload(Params, Coderlx) ->
    coderlx:request('config/mcpServer/reload', Params, Coderlx).
