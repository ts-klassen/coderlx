-module(coderlx_thread_section).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        list/2
      , create/2
      , update/2
      , delete/2
    ]).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_section_list_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_section_list_response)
      , {alias, {coderlx, coderlx}}
    ]}).
list(Params, Coderlx) ->
    coderlx:request('threadSection/list', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_section_create_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_section_create_response)
      , {alias, {coderlx, coderlx}}
    ]}).
create(Params, Coderlx) ->
    coderlx:request('threadSection/create', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_section_update_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_section_update_response)
      , {alias, {coderlx, coderlx}}
    ]}).
update(Params, Coderlx) ->
    coderlx:request('threadSection/update', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_section_delete_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_section_delete_response)
      , {alias, {coderlx, coderlx}}
    ]}).
delete(Params, Coderlx) ->
    coderlx:request('threadSection/delete', Params, Coderlx).
