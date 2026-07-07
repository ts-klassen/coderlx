-module(coderlx_fs).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        read_file/2
      , write_file/2
      , create_directory/2
      , get_metadata/2
      , read_directory/2
      , remove/2
      , copy/2
      , watch/2
      , unwatch/2
    ]).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, fs_read_file_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(fs_read_file_response)
      , {alias, {coderlx, coderlx}}
    ]}).
read_file(Params, Coderlx) ->
    coderlx:request('fs/readFile', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, fs_write_file_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(fs_write_file_response)
      , {alias, {coderlx, coderlx}}
    ]}).
write_file(Params, Coderlx) ->
    coderlx:request('fs/writeFile', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, fs_create_directory_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(fs_create_directory_response)
      , {alias, {coderlx, coderlx}}
    ]}).
create_directory(Params, Coderlx) ->
    coderlx:request('fs/createDirectory', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, fs_get_metadata_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(fs_get_metadata_response)
      , {alias, {coderlx, coderlx}}
    ]}).
get_metadata(Params, Coderlx) ->
    coderlx:request('fs/getMetadata', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, fs_read_directory_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(fs_read_directory_response)
      , {alias, {coderlx, coderlx}}
    ]}).
read_directory(Params, Coderlx) ->
    coderlx:request('fs/readDirectory', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, fs_remove_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(fs_remove_response)
      , {alias, {coderlx, coderlx}}
    ]}).
remove(Params, Coderlx) ->
    coderlx:request('fs/remove', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, fs_copy_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(fs_copy_response)
      , {alias, {coderlx, coderlx}}
    ]}).
copy(Params, Coderlx) ->
    coderlx:request('fs/copy', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, fs_watch_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(fs_watch_response)
      , {alias, {coderlx, coderlx}}
    ]}).
watch(Params, Coderlx) ->
    coderlx:request('fs/watch', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, fs_unwatch_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(fs_unwatch_response)
      , {alias, {coderlx, coderlx}}
    ]}).
unwatch(Params, Coderlx) ->
    coderlx:request('fs/unwatch', Params, Coderlx).
