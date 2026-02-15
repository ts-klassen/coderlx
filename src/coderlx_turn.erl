-module(coderlx_turn).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        start/2
      , steer/2
      , interrupt/2
    ]).

-export_type([
        id/0
    ]).

-type id() :: klsn:binstr().


-klsn_input_rule([
        {alias, {coderlx_app_server_rules, turn_start_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(turn_start_response)
      , {alias, {coderlx, coderlx}}
    ]}).
start(Params, Coderlx) ->
    coderlx:request('turn/start', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, turn_steer_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(turn_steer_response)
      , {alias, {coderlx, coderlx}}
    ]}).
steer(Params, Coderlx) ->
    coderlx:request('turn/steer', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, turn_interrupt_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(turn_interrupt_response)
      , {alias, {coderlx, coderlx}}
    ]}).
interrupt(Params, Coderlx) ->
    coderlx:request('turn/interrupt', Params, Coderlx).
