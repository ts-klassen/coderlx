-module(coderlx_experimental_feature).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        list/2
      , enablement_set/2
    ]).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, experimental_feature_list_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(experimental_feature_list_response)
      , {alias, {coderlx, coderlx}}
    ]}).
list(Params, Coderlx) ->
    coderlx:request('experimentalFeature/list', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, experimental_feature_enablement_set_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(experimental_feature_enablement_set_response)
      , {alias, {coderlx, coderlx}}
    ]}).
enablement_set(Params, Coderlx) ->
    coderlx:request('experimentalFeature/enablement/set', Params, Coderlx).
