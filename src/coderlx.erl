-module(coderlx).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        start/1
      , stop/1
      , default_codex_path/0
    ]).

%% Internal functions
-export([
        request/3
      , pop_message/3
    ]).

%% Temporary test functions for dev
-export([
        test/0
    ]).

-export_type([
        coderlx/0
      , opts/0
      , message/0
      , jsonrpc_error_or/1
    ]).

-klsn_rule_alias([
        {coderlx, {struct, #{
            '?MODULE' => {required, {exact, ?MODULE}}
          , stream => {required, term}
          , buffer => {required, binstr}
          , stderr => {required, term}
          , pending => {required, {list, term}}
          , next_id => {required, {range, {0, '<', integer}}}
          , request_timeout => {required, timeout}
        }}}
      , {opts, {struct, #{
            bwrap => {required, term}
          , codex_path => {optional, binstr}
        }}}
      , {message, {any_of, [
            {alias, {coderlx_app_server_rules, server_request}}
          , {alias, {coderlx_app_server_rules, codex_event_notification}}
          , {alias, {coderlx_app_server_rules, server_notification}}
          , {alias, {coderlx_app_server_rules, jsonrpc_response}}
          , {alias, {coderlx_app_server_rules, jsonrpc_error}}
        ]}}
    ]).

-opaque coderlx() :: klsn_rule:alias(coderlx).
-type opts() :: klsn_rule:alias(opts).
-type message() :: klsn_rule:alias(message).
-type jsonrpc_error_or(Response) :: {ok, Response} | {error, klsn_rule:alias(coderlx_app_server_rules, jsonrpc_error)}.

-spec start(opts()) -> coderlx().
start(#{bwrap := Bwrap}=Opts) ->
    {ok, _} = application:ensure_all_started(coderlx),
    Codex = maps:get(codex_path, Opts, default_codex_path()),
    Stream = klsn_bwrap:open([Codex, <<"app-server">>], #{
        bwrap => Bwrap
    }),
    #{
        '?MODULE' => ?MODULE
      , stream => Stream
      , buffer => <<>>
      , stderr => <<>>
      , pending => []
      , next_id => 1
      , request_timeout => 60000
    }.

-spec stop(coderlx()) -> ok.
stop(#{stream := Stream}) ->
    klsn_bwrap:stop(Stream).

request(Method, Params, Coderlx0) ->
    Id = maps:get(next_id, Coderlx0),
    Stream = maps:get(stream, Coderlx0),
    Message = #{
        id => Id,
        method => Method,
        params => Params
    },
    Normalized = klsn_rule:normalize(
        Message
      , {alias, {coderlx_app_server_rules, client_request}}
    ),
    JSON = klsn_binstr:from_any(jsone:encode(Normalized)),
    klsn_bwrap:send(Stream, <<JSON/binary, "\n">>),
    {Response, Coderlx} = pop_response(Id, Coderlx0),
    {Response, Coderlx#{
        next_id := Id + 1
    }}.

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, initialize_params}}
      , {alias, {?MODULE, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(initialize_response)
      , {alias, {?MODULE, coderlx}}
    ]}).
initialize(Params, Coderlx) ->
    request(initialize, Params, Coderlx).

-spec pop_response(
        pos_integer(), coderlx()
    ) -> {jsonrpc_error_or(term()), coderlx()}.
pop_response(Id, Coderlx0) ->
    case
        pop_message(
            fun
                (#{id := Id0}) when Id0 =:= Id ->
                    true;
                (_) ->
                    false
            end
          , maps:get(request_timeout, Coderlx0)
          , Coderlx0
        )
    of
        {{value, Resp}, Coderlx} ->
            %% Return result or jsonrpc error
            Either = case Resp of
                #{result := Result} ->
                    {ok, Result};
                _ ->
                    {error, Resp}
            end,
            {Either, Coderlx};
        {none, _} ->
            erlang:error(response_timeout, [Id, Coderlx0])
    end.


-spec pop_message(
        fun((message()) -> boolean())
      , timeout()
      , coderlx()
    ) -> {klsn:optnl(message()), coderlx()}.
pop_message(IsTarget, Timeout, Coderlx0) ->
    StartTime = erlang:monotonic_time(millisecond),
    Coderlx10 = dump_stream(Coderlx0),
    Coderlx20 = consume_buffer(Coderlx10),
    Pending0 = maps:get(pending, Coderlx20),
    case pop_matching_message_(IsTarget, Pending0, []) of
        {value, {Message, Pending}} ->
            {{value, Message}, Coderlx20#{ pending := Pending }};
        none ->
            case receive_stream(Timeout, Coderlx20) of
                {value, Coderlx30} ->
                    NextTimeout = case Timeout of
                        infinity -> infinity;
                        _ ->
                            max(0, Timeout - erlang:monotonic_time(millisecond) + StartTime)
                    end,
                    pop_message(IsTarget, NextTimeout, Coderlx30);
                none ->
                    {none, Coderlx20}
            end
    end.

pop_matching_message_(_, [], _) ->
    none;
pop_matching_message_(IsTarget, [H|T], Acc) ->
    case IsTarget(H) of
        true ->
            {value, {H, lists:reverse(Acc) ++ T}};
        false ->
            pop_matching_message_(IsTarget, T, [H|Acc])
    end.


dump_stream(Coderlx0) ->
    case receive_stream(0, Coderlx0) of
        {value, Coderlx} ->
            dump_stream(Coderlx);
        none ->
            Coderlx0
    end.

receive_stream(Timeout, Coderlx = #{stream := #{os_pid := OsPid}, buffer := StdOut, stderr := StdErr}) ->
    receive
        {stdout, OsPid, Data} ->
            {value, Coderlx#{
                buffer := <<StdOut/binary, Data/binary>>
              , stderr := StdErr
            }};
        {stderr, OsPid, Data} ->
            {value, Coderlx#{
                buffer := StdOut
              , stderr := <<StdErr/binary, Data/binary>>
            }};
        %% DOWN from erlexec. (Not monitor.)
        {'DOWN', OsPid, process, _Pid, Reason} ->
            erlang:error({codex_exit, Reason})
    after Timeout ->
        none
    end.

consume_buffer(Coderlx = #{ buffer := Data, pending := Pending}) ->
    Parts = binary:split(Data, <<"\n">>, [global]),
    {RevLines, Rest} = case lists:reverse(Parts) of
        [] ->
            {[], <<>>};
        [Last | RevLines0] ->
            {lists:reverse(RevLines0), Last}
    end,
    Messages = lists:foldr(fun(Line, Acc) ->
        case Line of
            <<>> ->
                Acc;
            _ ->
                [decode_message(Line) | Acc]
        end
    end, [], RevLines),
    Coderlx#{
        buffer := Rest
      , pending := Pending ++ Messages
    }.

decode_message(Line) ->
    Json = jsone:decode(Line),
    case classify_message(Json) of
        request ->
            klsn_rule:normalize(Json, {alias, {coderlx_app_server_rules, server_request}});
        notification ->
            case maps:get(<<"method">>, Json) of
                <<"codex/event/", _/binary>> ->
                    klsn_rule:normalize(
                        Json,
                        {alias, {coderlx_app_server_rules, codex_event_notification}}
                    );
                _ ->
                    klsn_rule:normalize(
                        Json,
                        {alias, {coderlx_app_server_rules, server_notification}}
                    )
            end;
        response ->
            klsn_rule:normalize(Json, {alias, {coderlx_app_server_rules, jsonrpc_response}});
        error ->
            klsn_rule:normalize(Json, {alias, {coderlx_app_server_rules, jsonrpc_error}});
        unknown ->
            erlang:error({unknown_message, Json})
    end.

classify_message(Json) when is_map(Json) ->
    HasMethod = maps:is_key(<<"method">>, Json),
    HasId = maps:is_key(<<"id">>, Json),
    HasResult = maps:is_key(<<"result">>, Json),
    HasError = maps:is_key(<<"error">>, Json),
    case {HasMethod, HasId, HasResult, HasError} of
        {true, true, _, _} -> request;
        {true, false, _, _} -> notification;
        {false, true, true, _} -> response;
        {false, true, _, true} -> error;
        _ -> unknown
    end;
classify_message(_) ->
    unknown.

-spec default_codex_path() -> klsn:binstr().
default_codex_path() ->
    klsn_binstr:from_any(filename:join([
        code:priv_dir(coderlx)
      , "codex"
    ])).


test() ->
    C0 = start(#{
        bwrap => [
            {tmpfs, <<"/">>}
          , {proc, <<"/proc">>}
          , {dev, <<"/dev">>}
          , {dir, <<"/tmp">>}
          , share_net
          , new_session
          , die_with_parent
          , clearenv
          , {setenv, <<"HOME">>, <<"/home/codex">>}
          , {setenv, <<"USER">>, <<"codex">>}
          , {setenv, <<"LOGNAME">>, <<"codex">>}
          , {setenv, <<"PATH">>, <<"/cbin:/usr/bin:/bin">>}
          , {setenv, <<"SHELL">>, <<"/bin/bash">>}
          , {setenv, <<"TMPDIR">>, <<"/tmp">>}
          , {dir, <<"/home">>}
          , {dir, <<"/home/codex">>}
          , {dir, <<"/home/codex/work">>}
          , {ro_bind, <<"/usr">>, <<"/usr">>}
          , {ro_bind, <<"/bin">>, <<"/bin">>}
          , {ro_bind, <<"/lib">>, <<"/lib">>}
          , {ro_bind, <<"/lib64">>, <<"/lib64">>}
          , {ro_bind, <<"/etc">>, <<"/etc">>}
          , {ro_bind, <<"/run">>, <<"/run">>}
          , {ro_bind, <<"/sys">>, <<"/sys">>}
          , {ro_bind, default_codex_path(), <<"/cbin/codex">>}
          , {bind, <<"/home/codex/.codex">>, <<"/home/codex/.codex">>}
        ]
      , codex_path => <<"/cbin/codex">>
    }),
    {Resp, C1} = initialize(#{clientInfo => #{name => coderlx, version => <<"0.1.0">>}}, C0),
    timer:sleep(1000),
    stop(C1),
    {Resp, C1}.
