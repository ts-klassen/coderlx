-module(coderlx_account).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        read/2
      , login_start/2
      , login_cancel/2
      , logout/2
      , rate_limits_read/2
    ]).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, account_read_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(account_read_response)
      , {alias, {coderlx, coderlx}}
    ]}).
read(Params, Coderlx) ->
    coderlx:request('account/read', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, account_login_start_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(account_login_start_response)
      , {alias, {coderlx, coderlx}}
    ]}).
login_start(Params, Coderlx) ->
    coderlx:request('account/login/start', Params, Coderlx).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, account_login_cancel_params}}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(account_login_cancel_response)
      , {alias, {coderlx, coderlx}}
    ]}).
login_cancel(Params, Coderlx) ->
    coderlx:request('account/login/cancel', Params, Coderlx).

-klsn_input_rule([
        {exact, null}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(account_logout_response)
      , {alias, {coderlx, coderlx}}
    ]}).
logout(Params, Coderlx) ->
    coderlx:request('account/logout', Params, Coderlx).

-klsn_input_rule([
        {exact, null}
      , {alias, {coderlx, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(account_rate_limits_read_response)
      , {alias, {coderlx, coderlx}}
    ]}).
rate_limits_read(Params, Coderlx) ->
    coderlx:request('account/rateLimits/read', Params, Coderlx).
