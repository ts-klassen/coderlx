-module(coderlx_plugin).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        list/2
      , installed/2
      , read/2
      , skill_read/2
      , share_save/2
      , share_update_targets/2
      , share_list/2
      , share_checkout/2
      , share_delete/2
      , install/2
      , uninstall/2
    ]).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, plugin_list_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(plugin_list_response)
      , {alias, {coderlx, coderlx}}
    ]}).
list(Params, Coderlx) ->
    coderlx:request('plugin/list', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, plugin_installed_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(plugin_installed_response)
      , {alias, {coderlx, coderlx}}
    ]}).
installed(Params, Coderlx) ->
    coderlx:request('plugin/installed', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, plugin_read_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(plugin_read_response)
      , {alias, {coderlx, coderlx}}
    ]}).
read(Params, Coderlx) ->
    coderlx:request('plugin/read', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, plugin_skill_read_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(plugin_skill_read_response)
      , {alias, {coderlx, coderlx}}
    ]}).
skill_read(Params, Coderlx) ->
    coderlx:request('plugin/skill/read', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, plugin_share_save_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(plugin_share_save_response)
      , {alias, {coderlx, coderlx}}
    ]}).
share_save(Params, Coderlx) ->
    coderlx:request('plugin/share/save', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, plugin_share_update_targets_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(plugin_share_update_targets_response)
      , {alias, {coderlx, coderlx}}
    ]}).
share_update_targets(Params, Coderlx) ->
    coderlx:request('plugin/share/updateTargets', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, plugin_share_list_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(plugin_share_list_response)
      , {alias, {coderlx, coderlx}}
    ]}).
share_list(Params, Coderlx) ->
    coderlx:request('plugin/share/list', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, plugin_share_checkout_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(plugin_share_checkout_response)
      , {alias, {coderlx, coderlx}}
    ]}).
share_checkout(Params, Coderlx) ->
    coderlx:request('plugin/share/checkout', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, plugin_share_delete_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(plugin_share_delete_response)
      , {alias, {coderlx, coderlx}}
    ]}).
share_delete(Params, Coderlx) ->
    coderlx:request('plugin/share/delete', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, plugin_install_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(plugin_install_response)
      , {alias, {coderlx, coderlx}}
    ]}).
install(Params, Coderlx) ->
    coderlx:request('plugin/install', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, plugin_uninstall_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(plugin_uninstall_response)
      , {alias, {coderlx, coderlx}}
    ]}).
uninstall(Params, Coderlx) ->
    coderlx:request('plugin/uninstall', Params, Coderlx).
