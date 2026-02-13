-module(coderlx_thread).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        start/2
    ]).

-export_type([
        id/0
    ]).

-klsn_rule_alias([
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
