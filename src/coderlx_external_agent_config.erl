-module(coderlx_external_agent_config).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        detect/2
      , import/2
      , import_read_histories/2
    ]).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, external_agent_config_detect_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(external_agent_config_detect_response)
      , {alias, {coderlx, coderlx}}
    ]}).
detect(Params, Coderlx) ->
    coderlx:request('externalAgentConfig/detect', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, external_agent_config_import_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(external_agent_config_import_response)
      , {alias, {coderlx, coderlx}}
    ]}).
import(Params, Coderlx) ->
    coderlx:request('externalAgentConfig/import', Params, Coderlx).

-klsn_input_rule([
        {exact, null}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(external_agent_config_import_read_histories_response)
      , {alias, {coderlx, coderlx}}
    ]}).
import_read_histories(Params, Coderlx) ->
    coderlx:request('externalAgentConfig/import/readHistories', Params, Coderlx).
