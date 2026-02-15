-module(coderlx_skills).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        list/2
      , config_write/2
    ]).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, skills_list_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(skills_list_response)
      , {alias, {coderlx, coderlx}}
    ]}).
list(Params, Coderlx) ->
    coderlx:request('skills/list', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, skills_config_write_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(skills_config_write_response)
      , {alias, {coderlx, coderlx}}
    ]}).
config_write(Params, Coderlx) ->
    coderlx:request('skills/config/write', Params, Coderlx).
