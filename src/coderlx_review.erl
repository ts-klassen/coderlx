-module(coderlx_review).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        start/2
    ]).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, review_start_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(review_start_response)
      , {alias, {coderlx, coderlx}}
    ]}).
start(Params, Coderlx) ->
    coderlx:request('review/start', Params, Coderlx).
