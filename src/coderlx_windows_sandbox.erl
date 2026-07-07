-module(coderlx_windows_sandbox).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        setup_start/2
      , readiness/2
    ]).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, windows_sandbox_setup_start_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(windows_sandbox_setup_start_response)
      , {alias, {coderlx, coderlx}}
    ]}).
setup_start(Params, Coderlx) ->
    coderlx:request('windowsSandbox/setupStart', Params, Coderlx).

-klsn_input_rule([
        {exact, null}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(windows_sandbox_readiness_response)
      , {alias, {coderlx, coderlx}}
    ]}).
readiness(Params, Coderlx) ->
    coderlx:request('windowsSandbox/readiness', Params, Coderlx).
