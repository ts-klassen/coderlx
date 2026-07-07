-module(coderlx_command).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        exec/2
      , exec_write/2
      , exec_terminate/2
      , exec_resize/2
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

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, command_exec_write_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(command_exec_write_response)
      , {alias, {coderlx, coderlx}}
    ]}).
exec_write(Params, Coderlx) ->
    coderlx:request('command/exec/write', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, command_exec_terminate_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(command_exec_terminate_response)
      , {alias, {coderlx, coderlx}}
    ]}).
exec_terminate(Params, Coderlx) ->
    coderlx:request('command/exec/terminate', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, command_exec_resize_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(command_exec_resize_response)
      , {alias, {coderlx, coderlx}}
    ]}).
exec_resize(Params, Coderlx) ->
    coderlx:request('command/exec/resize', Params, Coderlx).
