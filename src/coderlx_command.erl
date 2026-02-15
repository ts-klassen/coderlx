-module(coderlx_command).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        exec/2
    ]).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, command_exec_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(command_exec_response)
      , {alias, {coderlx, coderlx}}
    ]}).
exec(Params, Coderlx) ->
    coderlx:request('command/exec', Params, Coderlx).
