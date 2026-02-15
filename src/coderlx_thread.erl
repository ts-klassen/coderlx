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
