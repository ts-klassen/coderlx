-module(coderlx_apps).
%% API namespace uses app/list in the upstream protocol; module name is `apps`
%% to avoid confusion with Erlang application concepts and the `app-server` binary.
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        list/2
    ]).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, apps_list_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(apps_list_response)
      , {alias, {coderlx, coderlx}}
    ]}).
list(Params, Coderlx) ->
    coderlx:request('app/list', Params, Coderlx).
