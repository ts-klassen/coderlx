-module(coderlx_transport_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([suite/0, all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
    request_send_timeout/1,
    success_reply_send_timeout/1,
    error_reply_send_timeout/1,
    request_and_replies/1,
    response_timeout/1,
    stop_preserves_other_errors/1
]).

suite() ->
    [{timetrap, {seconds, 15}}].

all() ->
    [
        request_send_timeout,
        success_reply_send_timeout,
        error_reply_send_timeout,
        request_and_replies,
        response_timeout,
        stop_preserves_other_errors
    ].

init_per_testcase(stop_preserves_other_errors, Config) ->
    Config;
init_per_testcase(TestCase, Config) ->
    {Script, ExtraOpts} = case TestCase of
        request_and_replies -> {"echo_codex.sh", #{}};
        response_timeout -> {"blocked_codex.sh", #{send_timeout => 5000}};
        _ -> {"blocked_codex.sh", #{send_timeout => 100}}
    end,
    ScriptPath = unicode:characters_to_binary(
        filename:join(?config(data_dir, Config), Script)
    ),
    Hnd = coderlx:start(ExtraOpts#{
        app_server_path => <<"/tmp/app-server">>,
        bwrap => [
            {ro_bind, <<"/">>, <<"/">>},
            {tmpfs, <<"/tmp">>},
            {proc, <<"/proc">>},
            {dev, <<"/dev">>},
            {ro_bind, ScriptPath, <<"/tmp/app-server">>},
            {chdir, <<"/tmp">>}
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

end_per_testcase(stop_preserves_other_errors, _Config) ->
    ok;
end_per_testcase(_TestCase, Config) ->
    coderlx:stop(?config(coderlx, Config)).

request_send_timeout(Config) ->
    Params = #{clientInfo => #{name => large_input(), version => <<"0.1.0">>}},
    assert_send_timeout(Config, fun(Hnd) ->
        coderlx:initialize(Params, Hnd)
    end).

success_reply_send_timeout(Config) ->
    Result = #{
        contentItems => [#{type => inputText, text => large_input()}],
        success => true
    },
    assert_send_timeout(Config, fun(Hnd) ->
        coderlx:respond_ok(<<"tool-1">>, Result, Hnd)
    end).

error_reply_send_timeout(Config) ->
    Reason = #{details => large_input()},
    assert_send_timeout(Config, fun(Hnd) ->
        coderlx:respond_error(<<"tool-1">>, consumer_error, Reason, Hnd)
    end).

assert_send_timeout(Config, Send) ->
    Hnd = ?config(coderlx, Config),
    #{
        stream := #{exec_pid := Worker},
        send_timeout := 100,
        request_timeout := 60000
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

request_and_replies(Config) ->
    Hnd0 = ?config(coderlx, Config),
    ?assertEqual(60000, maps:get(send_timeout, Hnd0)),
    Params = #{clientInfo => #{name => <<"transport-test">>, version => <<"0.1.0">>}},
    {{ok, #{}}, Hnd1} = coderlx:request(initialize, Params, Hnd0),
    ?assertEqual(2, maps:get(next_id, Hnd1)),
    #{stream := #{os_pid := OsPid}} = Hnd1,
    Request = jsone:decode(stderr_line(OsPid, maps:get(stderr, Hnd1))),
    ?assertMatch(#{
        <<"id">> := 1,
        <<"method">> := <<"initialize">>,
        <<"params">> := #{<<"clientInfo">> := #{<<"name">> := <<"transport-test">>}}
    }, Request),
    ok = coderlx:respond_ok(<<"approval-1">>, #{decision => accept}, Hnd1),
    ?assertEqual(#{
        <<"id">> => <<"approval-1">>,
        <<"result">> => #{<<"decision">> => <<"accept">>}
    }, jsone:decode(stderr_line(OsPid, <<>>))),
    ok = coderlx:respond_error(<<"tool-1">>, consumer_error, #{detail => <<"failed">>}, Hnd1),
    ?assertMatch(#{
        <<"id">> := <<"tool-1">>,
        <<"error">> := #{
            <<"code">> := -32000,
            <<"data">> := #{<<"detail">> := <<"failed">>}
        }
    }, jsone:decode(stderr_line(OsPid, <<>>))).

response_timeout(Config) ->
    Hnd = (?config(coderlx, Config))#{request_timeout := 20},
    #{stream := #{exec_pid := Worker}} = Hnd,
    Params = #{clientInfo => #{name => <<"transport-test">>, version => <<"0.1.0">>}},
    %% A small request fits the input buffer. Only the response wait expires.
    ?assertError(response_timeout, coderlx:initialize(Params, Hnd)),
    ?assert(is_process_alive(Worker)).

stop_preserves_other_errors(_Config) ->
    ?assertError(badarg, coderlx:stop(#{stream => #{}})).

large_input() ->
    binary:copy(<<"x">>, 8 * 1024 * 1024).

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
