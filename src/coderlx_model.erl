-module(coderlx_model).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        list/2
      , provider_capabilities_read/2
    ]).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, model_list_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(model_list_response)
      , {alias, {coderlx, coderlx}}
    ]}).
list(Params, Coderlx) ->
    coderlx:request('model/list', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, model_provider_capabilities_read_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(model_provider_capabilities_read_response)
      , {alias, {coderlx, coderlx}}
    ]}).
provider_capabilities_read(Params, Coderlx) ->
    coderlx:request('modelProvider/capabilities/read', Params, Coderlx).
