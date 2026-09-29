-module(coderlx_transport_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([suite/0, all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
        request_send_timeout/1
      , success_reply_send_timeout/1
      , error_reply_send_timeout/1
      , request_and_replies/1
      , response_with_same_id_request/1
      , messages_before_normal_exit/1
      , messages_before_failed_exit/1
      , response_timeout/1
      , stop_preserves_other_errors/1
    ]).

-type test_case() :: request_send_timeout
                  | success_reply_send_timeout
                  | error_reply_send_timeout
                  | request_and_replies
                  | response_with_same_id_request
                  | messages_before_normal_exit
                  | messages_before_failed_exit
                  | response_timeout
                  | stop_preserves_other_errors.

-spec suite() -> [{timetrap, {seconds, pos_integer()}}].
suite() ->
    [{timetrap, {seconds, 15}}].

-spec all() -> [test_case()].
all() ->
    [
        request_send_timeout
      , success_reply_send_timeout
      , error_reply_send_timeout
      , request_and_replies
      , response_with_same_id_request
      , messages_before_normal_exit
      , messages_before_failed_exit
      , response_timeout
      , stop_preserves_other_errors
    ].

-spec init_per_testcase(test_case(), proplists:proplist()) -> proplists:proplist().
init_per_testcase(stop_preserves_other_errors, Config) ->
    Config;
init_per_testcase(TestCase, Config) ->
    {Script, ExtraOpts} = case TestCase of
        request_and_replies -> {"echo_codex.sh", #{}};
        response_with_same_id_request -> {"collision_codex.sh", #{}};
        messages_before_normal_exit -> {"exit_codex.sh", #{}};
        messages_before_failed_exit -> {"exit_codex.sh", #{}};
        response_timeout -> {"blocked_codex.sh", #{send_timeout => 5000}};
        _ -> {"blocked_codex.sh", #{send_timeout => 100}}
    end,
    ScriptPath = unicode:characters_to_binary(
        filename:join(?config(data_dir, Config), Script)
    ),
    Hnd = coderlx:start(ExtraOpts#{
        app_server_path => <<"/tmp/app-server">>
      , bwrap => [
            {ro_bind, <<"/">>, <<"/">>}
          , {tmpfs, <<"/tmp">>}
          , {proc, <<"/proc">>}
          , {dev, <<"/dev">>}
          , {ro_bind, ScriptPath, <<"/tmp/app-server">>}
          , {chdir, <<"/tmp">>}
        ]
    }),
    #{stream := #{os_pid := OsPid}} = Hnd,
    try
        <<"ready">> = stderr_line(OsPid, <<>>),
        [{coderlx, Hnd} | Config]
    catch
        Class:Reason:Stack ->
            coderlx:stop(Hnd),
            erlang:raise(Class, Reason, Stack)
    end.

-spec end_per_testcase(test_case(), proplists:proplist()) -> ok.
end_per_testcase(stop_preserves_other_errors, _Config) ->
    ok;
end_per_testcase(_TestCase, Config) ->
    coderlx:stop(?config(coderlx, Config)).

-spec request_send_timeout(proplists:proplist()) -> ok.
request_send_timeout(Config) ->
    Params = #{clientInfo => #{name => large_input(), version => <<"0.1.0">>}},
    assert_send_timeout(Config, fun(Hnd) ->
        coderlx:initialize(Params, Hnd)
    end).

-spec success_reply_send_timeout(proplists:proplist()) -> ok.
success_reply_send_timeout(Config) ->
    Result = #{
        contentItems => [#{type => inputText, text => large_input()}]
      , success => true
    },
    assert_send_timeout(Config, fun(Hnd) ->
        coderlx:respond_ok(<<"tool-1">>, Result, Hnd)
    end).

-spec error_reply_send_timeout(proplists:proplist()) -> ok.
error_reply_send_timeout(Config) ->
    Reason = #{details => large_input()},
    assert_send_timeout(Config, fun(Hnd) ->
        coderlx:respond_error(<<"tool-1">>, consumer_error, Reason, Hnd)
    end).

-spec request_and_replies(proplists:proplist()) -> ok.
request_and_replies(Config) ->
    Hnd0 = ?config(coderlx, Config),
    ?assertEqual(60000, maps:get(send_timeout, Hnd0)),
    ?assertEqual(none, klsn_map:lookup([stream_exit], Hnd0)),
    ?assertEqual(none, klsn_map:lookup([server_request_handler], Hnd0)),
    Params = #{clientInfo => #{name => <<"transport-test">>, version => <<"0.1.0">>}},
    {{ok, #{}}, Hnd1} = coderlx:request(initialize, Params, Hnd0),
    ?assertEqual(2, maps:get(next_id, Hnd1)),
    #{stream := #{os_pid := OsPid}} = Hnd1,
    Request = jsone:decode(stderr_line(OsPid, maps:get(stderr, Hnd1))),
    ?assertMatch(#{
        <<"id">> := 1
      , <<"method">> := <<"initialize">>
      , <<"params">> := #{<<"clientInfo">> := #{<<"name">> := <<"transport-test">>}}
    }, Request),
    ok = coderlx:respond_ok(<<"approval-1">>, #{decision => accept}, Hnd1),
    ?assertEqual(#{
        <<"id">> => <<"approval-1">>
      , <<"result">> => #{<<"decision">> => <<"accept">>}
    }, jsone:decode(stderr_line(OsPid, <<>>))),
    ok = coderlx:respond_error(<<"tool-1">>, consumer_error, #{detail => <<"failed">>}, Hnd1),
    ?assertMatch(#{
        <<"id">> := <<"tool-1">>
      , <<"error">> := #{
            <<"code">> := -32000
          , <<"data">> := #{<<"detail">> := <<"failed">>}
        }
    }, jsone:decode(stderr_line(OsPid, <<>>))).

-spec response_with_same_id_request(proplists:proplist()) -> ok.
response_with_same_id_request(Config) ->
    Hnd0 = ?config(coderlx, Config),
    Params = #{clientInfo => #{name => <<"transport-test">>, version => <<"0.1.0">>}},
    {{ok, #{}}, Hnd1} = coderlx:request(initialize, Params, Hnd0),
    ?assertEqual(2, maps:get(next_id, Hnd1)),
    {{value, #{id := 1, method := 'attestation/generate'}}, _} =
        coderlx:pop_message(fun(_) -> true end, 0, Hnd1),
    ok.

-spec messages_before_normal_exit(proplists:proplist()) -> ok.
messages_before_normal_exit(Config) ->
    messages_before_exit(Config, 0, normal).

-spec messages_before_failed_exit(proplists:proplist()) -> ok.
messages_before_failed_exit(Config) ->
    messages_before_exit(Config, 7, {exit_status, 7 bsl 8}).

-spec response_timeout(proplists:proplist()) -> ok.
response_timeout(Config) ->
    Hnd = (?config(coderlx, Config))#{request_timeout := 20},
    #{stream := #{exec_pid := Worker}} = Hnd,
    Params = #{clientInfo => #{name => <<"transport-test">>, version => <<"0.1.0">>}},
    %% A small request fits the input buffer. Only the response wait expires.
    ?assertError(response_timeout, coderlx:initialize(Params, Hnd)),
    ?assert(is_process_alive(Worker)).

-spec stop_preserves_other_errors(proplists:proplist()) -> ok.
stop_preserves_other_errors(_Config) ->
    ?assertError(badarg, coderlx:stop(#{stream => #{}})).

-spec assert_send_timeout(
        proplists:proplist()
      , fun((coderlx:coderlx()) -> ok |
            {coderlx:jsonrpc_error_or(jsone:json_value()), coderlx:coderlx()})
    ) -> ok.
assert_send_timeout(Config, Send) ->
    Hnd = ?config(coderlx, Config),
    #{
        stream := #{exec_pid := Worker}
      , send_timeout := 100
      , request_timeout := 60000
    } = Hnd,
    Start = erlang:monotonic_time(millisecond),
    %% Exercise callers' usual try/after cleanup: a stopped transport must
    %% not replace error:timeout with error:{badmatch, {error, no_process}}.
    ?assertError(timeout, try Send(Hnd) after coderlx:stop(Hnd) end),
    Elapsed = erlang:monotonic_time(millisecond) - Start,
    ?assert(Elapsed >= 100),
    ?assert(Elapsed < 5000),
    ?assertNot(is_process_alive(Worker)),
    ?assertEqual(ok, coderlx:stop(Hnd)).

-spec large_input() -> binary().
large_input() ->
    binary:copy(<<"x">>, 8 * 1024 * 1024).

-spec stderr_line(integer(), binary()) -> binary().
stderr_line(OsPid, Buffer) ->
    case binary:split(Buffer, <<"\n">>) of
        [Line, <<>>] -> Line;
        [_] ->
            receive
                {stderr, OsPid, Data} ->
                    stderr_line(OsPid, <<Buffer/binary, Data/binary>>);
                {'DOWN', OsPid, process, _, Reason} ->
                    ct:fail({server_exited, Reason})
            after 5000 ->
                ct:fail(stderr_timeout)
            end
    end.

-spec messages_before_exit(
        proplists:proplist()
      , non_neg_integer()
      , normal | {exit_status, non_neg_integer()}
    ) -> ok.
messages_before_exit(Config, ExitCode, Reason) ->
    Hnd0 = ?config(coderlx, Config),
    #{stream := Stream = #{exec_pid := Worker}} = Hnd0,
    Monitor = erlang:monitor(process, Worker),
    ok = klsn_bwrap:send(Stream, <<(integer_to_binary(ExitCode))/binary, "\n">>, 5000),
    %% Wait for process completion without consuming its stdout or stream DOWN.
    receive {'DOWN', Monitor, process, Worker, _} -> ok
    after 5000 -> ct:fail(server_did_not_exit)
    end,
    {{value, #{id := 1}}, Hnd1} = coderlx:pop_message(fun(_) -> true end, 0, Hnd0),
    {{value, #{id := 2}}, Hnd2} = coderlx:pop_message(fun(_) -> true end, 0, Hnd1),
    ?assertEqual({value, Reason}, klsn_map:lookup([stream_exit], Hnd2)),
    ?assertError({codex_exit, Reason},
        coderlx:pop_message(fun(_) -> true end, 0, Hnd2)).
