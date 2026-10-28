-module(coderlx_feedback).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

-export([save/2]).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, feedback_save_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(feedback_save_response)
      , {alias, {coderlx, coderlx}}
    ]}).
save(Params, Coderlx) ->
    coderlx:request('feedback/save', Params, Coderlx).
