-module(coderlx_thread).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        start/2
      , resume/2
      , fork/2
      , list/2
      , loaded_list/2
      , read/2
      , archive/2
      , name_set/2
      , unarchive/2
      , compact_start/2
      , rollback/2
      , delete/2
      , unsubscribe/2
      , goal_set/2
      , goal_get/2
      , goal_clear/2
      , metadata_update/2
      , shell_command/2
      , approve_guardian_denied_action/2
      , inject_items/2
    ]).

-export_type([
        id/0
    ]).

-type id() :: klsn:binstr().


-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_start_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_start_response)
      , {alias, {coderlx, coderlx}}
    ]}).
start(Params, Coderlx) ->
    coderlx:request('thread/start', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_resume_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_resume_response)
      , {alias, {coderlx, coderlx}}
    ]}).
resume(Params, Coderlx) ->
    coderlx:request('thread/resume', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_fork_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_fork_response)
      , {alias, {coderlx, coderlx}}
    ]}).
fork(Params, Coderlx) ->
    coderlx:request('thread/fork', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_list_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_list_response)
      , {alias, {coderlx, coderlx}}
    ]}).
list(Params, Coderlx) ->
    coderlx:request('thread/list', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_loaded_list_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_loaded_list_response)
      , {alias, {coderlx, coderlx}}
    ]}).
loaded_list(Params, Coderlx) ->
    coderlx:request('thread/loaded/list', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_read_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_read_response)
      , {alias, {coderlx, coderlx}}
    ]}).
read(Params, Coderlx) ->
    coderlx:request('thread/read', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_archive_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_archive_response)
      , {alias, {coderlx, coderlx}}
    ]}).
archive(Params, Coderlx) ->
    coderlx:request('thread/archive', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_set_name_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_set_name_response)
      , {alias, {coderlx, coderlx}}
    ]}).
name_set(Params, Coderlx) ->
    coderlx:request('thread/name/set', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_unarchive_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_unarchive_response)
      , {alias, {coderlx, coderlx}}
    ]}).
unarchive(Params, Coderlx) ->
    coderlx:request('thread/unarchive', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_compact_start_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_compact_start_response)
      , {alias, {coderlx, coderlx}}
    ]}).
compact_start(Params, Coderlx) ->
    coderlx:request('thread/compact/start', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_rollback_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_rollback_response)
      , {alias, {coderlx, coderlx}}
    ]}).
rollback(Params, Coderlx) ->
    coderlx:request('thread/rollback', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_delete_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_delete_response)
      , {alias, {coderlx, coderlx}}
    ]}).
delete(Params, Coderlx) ->
    coderlx:request('thread/delete', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_unsubscribe_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_unsubscribe_response)
      , {alias, {coderlx, coderlx}}
    ]}).
unsubscribe(Params, Coderlx) ->
    coderlx:request('thread/unsubscribe', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_goal_set_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_goal_set_response)
      , {alias, {coderlx, coderlx}}
    ]}).
goal_set(Params, Coderlx) ->
    coderlx:request('thread/goal/set', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_goal_get_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_goal_get_response)
      , {alias, {coderlx, coderlx}}
    ]}).
goal_get(Params, Coderlx) ->
    coderlx:request('thread/goal/get', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_goal_clear_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_goal_clear_response)
      , {alias, {coderlx, coderlx}}
    ]}).
goal_clear(Params, Coderlx) ->
    coderlx:request('thread/goal/clear', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_metadata_update_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_metadata_update_response)
      , {alias, {coderlx, coderlx}}
    ]}).
metadata_update(Params, Coderlx) ->
    coderlx:request('thread/metadata/update', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_shell_command_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_shell_command_response)
      , {alias, {coderlx, coderlx}}
    ]}).
shell_command(Params, Coderlx) ->
    coderlx:request('thread/shellCommand', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_approve_guardian_denied_action_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_approve_guardian_denied_action_response)
      , {alias, {coderlx, coderlx}}
    ]}).
approve_guardian_denied_action(Params, Coderlx) ->
    coderlx:request('thread/approveGuardianDeniedAction', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, thread_inject_items_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(thread_inject_items_response)
      , {alias, {coderlx, coderlx}}
    ]}).
inject_items(Params, Coderlx) ->
    coderlx:request('thread/inject_items', Params, Coderlx).
