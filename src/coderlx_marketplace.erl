-module(coderlx_marketplace).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        add/2
      , remove/2
      , upgrade/2
    ]).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, marketplace_add_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(marketplace_add_response)
      , {alias, {coderlx, coderlx}}
    ]}).
add(Params, Coderlx) ->
    coderlx:request('marketplace/add', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, marketplace_remove_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(marketplace_remove_response)
      , {alias, {coderlx, coderlx}}
    ]}).
remove(Params, Coderlx) ->
    coderlx:request('marketplace/remove', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, marketplace_upgrade_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(marketplace_upgrade_response)
      , {alias, {coderlx, coderlx}}
    ]}).
upgrade(Params, Coderlx) ->
    coderlx:request('marketplace/upgrade', Params, Coderlx).
