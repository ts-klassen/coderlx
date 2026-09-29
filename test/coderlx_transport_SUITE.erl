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
      , response_timeout_preserves_state/1
      , handled_same_id_request/1
      , mcp_elicitation/1
      , mcp_infinite_timeout/1
      , mcp_unhandled/1
      , mcp_consumer_error/1
      , mcp_consumer_error_data/1
      , mcp_handler_failure/1
      , mcp_timeout_recovers_question/1
      , response_deadline/1
      , expired_handler_preserves_order/1
      , stop_preserves_other_errors/1
      , invalid_handler_option/1
      , mcp_success_encoding_failure/1
      , mcp_error_encoding_failure/1
      , fixture_dependency_under_tmp/1
    ]).

-type test_case() :: request_send_timeout
                      | success_reply_send_timeout
                      | error_reply_send_timeout
                      | request_and_replies
                      | response_with_same_id_request
                      | messages_before_normal_exit
                      | messages_before_failed_exit
                      | response_timeout
                      | response_timeout_preserves_state
                      | handled_same_id_request
                      | mcp_elicitation
                      | mcp_infinite_timeout
                      | mcp_unhandled
                      | mcp_consumer_error
                      | mcp_consumer_error_data
                      | mcp_handler_failure
                      | mcp_timeout_recovers_question
                      | response_deadline
                      | expired_handler_preserves_order
                      | stop_preserves_other_errors
                      | invalid_handler_option
                      | mcp_success_encoding_failure
                      | mcp_error_encoding_failure
                      | fixture_dependency_under_tmp.

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
      , response_timeout_preserves_state
      , handled_same_id_request
      , mcp_elicitation
      , mcp_infinite_timeout
      , mcp_unhandled
      , mcp_consumer_error
      , mcp_consumer_error_data
      , mcp_handler_failure
      , mcp_timeout_recovers_question
      , response_deadline
      , expired_handler_preserves_order
      , stop_preserves_other_errors
      , invalid_handler_option
      , mcp_success_encoding_failure
      , mcp_error_encoding_failure
      , fixture_dependency_under_tmp
    ].

-spec init_per_testcase(test_case(), proplists:proplist()) -> proplists:proplist().
init_per_testcase(TestCase, Config)
    when TestCase =:= stop_preserves_other_errors;
         TestCase =:= invalid_handler_option;
         TestCase =:= fixture_dependency_under_tmp ->
    Config;
init_per_testcase(TestCase, Config) ->
    {Script, ExtraOpts} = case TestCase of
        request_and_replies -> {"echo_codex.sh", #{}};
        response_with_same_id_request -> {"collision_codex.sh", #{}};
        handled_same_id_request -> {"collision_codex.sh", #{}};
        messages_before_normal_exit -> {"exit_codex.sh", #{}};
        messages_before_failed_exit -> {"exit_codex.sh", #{}};
        response_timeout -> {"blocked_codex.sh", #{send_timeout => 5000}};
        mcp_success_encoding_failure -> {"interactive_codex.escript", #{
            server_request_handler => fun(_) ->
                {ok, #{action => accept, content => #{42 => <<"invalid key">>}}}
            end}};
        mcp_error_encoding_failure -> {"interactive_codex.escript", #{
            server_request_handler => fun(_) ->
                {error, #{42 => <<"invalid key">>}}
            end}};
        response_timeout_preserves_state -> {"interactive_codex.escript", #{}};
        mcp_elicitation -> {"interactive_codex.escript", #{
            server_request_handler => fun({
                    request
                  , 'mcpServer/elicitation/request'
                  , #{mode := form, requestedSchema := #{type := object}}
                }) ->
                    {ok, #{action => accept, content => #{answer => <<"ok">>}}}
                end}};
        mcp_unhandled -> {"interactive_codex.escript", #{
            server_request_handler => fun(_) -> unhandled end}};
        mcp_consumer_error -> {"interactive_codex.escript", #{
            server_request_handler => fun(_) -> error end}};
        mcp_consumer_error_data -> {"interactive_codex.escript", #{
            server_request_handler => fun(_) ->
                {error, #{detail => <<"declined by caller">>}}
            end}};
        mcp_handler_failure -> {"interactive_codex.escript", #{
            server_request_handler => fun(_) ->
                erlang:error(simulated_failure, [handler_stack_marker])
            end}};
        mcp_timeout_recovers_question -> {"interactive_codex.escript", #{}};
        mcp_infinite_timeout -> {"interactive_codex.escript", #{}};
        response_deadline -> {"interactive_codex.escript", #{
            server_request_handler => fun(_) ->
                timer:sleep(30),
                {ok, #{token => <<"test-token">>}}
            end}};
        _ -> {"blocked_codex.sh", #{send_timeout => 100}}
    end,
    ScriptPath = unicode:characters_to_binary(
        filename:join(?config(data_dir, Config), Script)
    ),
    JsonEbin = unicode:characters_to_binary(proplists:get_value(
        json_ebin
      , Config
      , filename:join(code:lib_dir(jsone), "ebin")
    )),
    Hnd = coderlx:start(ExtraOpts#{
        app_server_path => <<"/tmp/app-server">>
      , bwrap => [
            {ro_bind, <<"/">>, <<"/">>}
          , {tmpfs, <<"/tmp">>}
          , {proc, <<"/proc">>}
          , {dev, <<"/dev">>}
          , {ro_bind, ScriptPath, <<"/tmp/app-server">>}
          , {ro_bind, JsonEbin, <<"/tmp/jsone-ebin">>}
          , {chdir, <<"/tmp">>}
          , {setenv, <<"CODERLX_TEST_JSON_EBIN">>, <<"/tmp/jsone-ebin">>}
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
end_per_testcase(TestCase, _Config)
    when TestCase =:= stop_preserves_other_errors;
         TestCase =:= invalid_handler_option;
         TestCase =:= fixture_dependency_under_tmp ->
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
    ?assertError(badarg, coderlx:respond_ok(
        7
      , #{action => accept, content => #{1 => <<"invalid key">>}}
      , Hnd1
    )),
    %% Rejected input must leave the stream usable and emit no partial reply.
    ok = coderlx:respond_ok(7, #{action => decline}, Hnd1),
    ?assertEqual(#{
        <<"id">> => 7
      , <<"result">> => #{<<"action">> => <<"decline">>}
    }, jsone:decode(stderr_line(OsPid, <<>>))),
    ?assertError(badarg, coderlx:respond_error(
        <<"invalid-error">>
      , consumer_error
      , #{1 => <<"invalid key">>}
      , Hnd1
    )),
    lists:foreach(fun({Id, Kind, Code}) ->
        ok = coderlx:respond_error(Id, Kind, #{detail => <<"failed">>}, Hnd1),
        ?assertMatch(#{
            <<"id">> := Id
          , <<"error">> := #{
                <<"code">> := Code
              , <<"message">> := <<_, _/binary>>
              , <<"data">> := #{<<"detail">> := <<"failed">>}
            }
        }, jsone:decode(stderr_line(OsPid, <<>>)))
    end, [
        {<<"tool-unsupported">>, unsupported, -32601}
      , {8, consumer_error, -32000}
      , {<<"tool-unhandled">>, unhandled, -32001}
    ]).

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

-spec response_timeout(proplists:proplist()) -> ok.
response_timeout(Config) ->
    Hnd = (?config(coderlx, Config))#{request_timeout := 20},
    #{stream := #{exec_pid := Worker}} = Hnd,
    Params = #{clientInfo => #{name => <<"transport-test">>, version => <<"0.1.0">>}},
    %% A small request fits the input buffer. Only the response wait expires.
    Recovered = timed_out(fun() -> coderlx:initialize(Params, Hnd) end, 1),
    ?assertEqual(2, maps:get(next_id, Recovered)),
    ?assert(is_process_alive(Worker)).

-spec response_timeout_preserves_state(proplists:proplist()) -> ok.
response_timeout_preserves_state(Config) ->
    Hnd0 = (?config(coderlx, Config))#{request_timeout := 100},
    Params = #{clientInfo => #{name => <<"timeout-data">>, version => <<"0.1.0">>}},
    Hnd1 = timed_out(fun() -> coderlx:request(initialize, Params, Hnd0) end, 1),
    ?assertEqual(2, maps:get(next_id, Hnd1)),
    ?assertEqual(<<"{\"id\":1,\"result\":">>, maps:get(buffer, Hnd1)),
    ?assertEqual(<<"timeout-data\n">>, maps:get(stderr, Hnd1)),
    {{value, #{method := 'fs/changed'}}, Hnd2} =
        coderlx:pop_message(fun(_) -> true end, 0, Hnd1),
    {{ok, #{}}, Hnd3} = coderlx:request(initialize, Params, Hnd2#{request_timeout := 1000}),
    ?assertEqual(3, maps:get(next_id, Hnd3)),
    {{value, #{id := 1, result := #{}}}, _} =
        coderlx:pop_message(fun(_) -> true end, 0, Hnd3),
    ok.

-spec handled_same_id_request(proplists:proplist()) -> ok.
handled_same_id_request(Config) ->
    Owner = self(),
    Handler = fun({request, 'attestation/generate', #{}}) ->
        Owner ! same_id_request_handled,
        {ok, #{token => <<"test-token">>}}
    end,
    Hnd0 = (?config(coderlx, Config))#{server_request_handler => Handler},
    Params = #{clientInfo => #{name => <<"transport-test">>, version => <<"0.1.0">>}},
    {{ok, #{}}, Hnd1} = coderlx:request(initialize, Params, Hnd0),
    receive same_id_request_handled -> ok
    after 0 -> ct:fail(handler_not_called)
    end,
    ?assertEqual([], maps:get(pending, Hnd1)).

-spec mcp_elicitation(proplists:proplist()) -> ok.
mcp_elicitation(Config) ->
    Hnd0 = ?config(coderlx, Config),
    {{ok, #{structuredContent := #{<<"result">> := Result}}}, Hnd1} =
        coderlx_mcp:server_tool_call(tool_params(<<"elicitation">>), Hnd0),
    ?assertMatch(#{
        <<"action">> := <<"accept">>
      , <<"content">> := #{<<"answer">> := <<"ok">>}
    }, Result),
    ?assert(is_function(maps:get(server_request_handler, Hnd1), 1)).

-spec mcp_infinite_timeout(proplists:proplist()) -> ok.
mcp_infinite_timeout(Config) ->
    Owner = self(),
    Hnd = (?config(coderlx, Config))#{
        request_timeout := infinity
      , server_request_handler => fun({request, 'attestation/generate', #{}}) ->
            Owner ! attestation_handled,
            {ok, #{token => <<"test-token">>}}
        end
    },
    {{ok, #{content := []}}, _} =
        coderlx_mcp:server_tool_call(tool_params(<<"deadline">>), Hnd),
    lists:foreach(fun(_) ->
        receive attestation_handled -> ok
        after 0 -> ct:fail(missing_attestation_callback)
        end
    end, lists:seq(1, 5)),
    receive attestation_handled -> ct:fail(unexpected_attestation_callback)
    after 0 -> ok
    end.

-spec mcp_unhandled(proplists:proplist()) -> ok.
mcp_unhandled(Config) ->
    Hnd = ?config(coderlx, Config),
    {{ok, #{structuredContent := #{<<"error">> := #{<<"code">> := -32001}}}}, _} =
        coderlx_mcp:server_tool_call(tool_params(<<"elicitation">>), Hnd),
    ok.

-spec mcp_consumer_error(proplists:proplist()) -> ok.
mcp_consumer_error(Config) ->
    Hnd = ?config(coderlx, Config),
    {{ok, #{structuredContent := #{<<"error">> := Error}}}, _} =
        coderlx_mcp:server_tool_call(tool_params(<<"elicitation">>), Hnd),
    ?assertEqual(-32000, maps:get(<<"code">>, Error)),
    ?assertEqual(#{}, maps:get(<<"data">>, Error)).

-spec mcp_consumer_error_data(proplists:proplist()) -> ok.
mcp_consumer_error_data(Config) ->
    Hnd = ?config(coderlx, Config),
    {{ok, #{structuredContent := #{<<"error">> := Error}}}, _} =
        coderlx_mcp:server_tool_call(tool_params(<<"elicitation">>), Hnd),
    ?assertEqual(-32000, maps:get(<<"code">>, Error)),
    ?assertEqual(#{<<"detail">> => <<"declined by caller">>}, maps:get(<<"data">>, Error)).

-spec mcp_handler_failure(proplists:proplist()) -> ok.
mcp_handler_failure(Config) ->
    Hnd0 = ?config(coderlx, Config),
    Hnd1 = try
        coderlx_mcp:server_tool_call(tool_params(<<"elicitation">>), Hnd0),
        ct:fail(expected_handler_failure)
    catch
        error:{server_request_handler_failed, 0, error, simulated_failure, Recovered}:Stack ->
            %% The original error arguments identify the callback's frame.
            ?assertMatch([{?MODULE, _, [handler_stack_marker], _} | _], Stack),
            Recovered
    end,
    ?assertEqual(2, maps:get(next_id, Hnd1)),
    {{value, #{result := #{<<"structuredContent">> :=
        #{<<"error">> := #{<<"code">> := -32000}}}}}, _} =
        coderlx:pop_message(
            fun(#{id := 1, result := _}) -> true; (_) -> false end
          , 1000
          , Hnd1
        ),
    ok.

-spec mcp_timeout_recovers_question(proplists:proplist()) -> ok.
mcp_timeout_recovers_question(Config) ->
    Hnd0 = (?config(coderlx, Config))#{request_timeout := 100},
    Hnd1 = timed_out(fun() ->
        coderlx_mcp:server_tool_call(tool_params(<<"elicitation">>), Hnd0)
    end, 1),
    Consumer = fun
        ({request, 'mcpServer/elicitation/request', #{mode := form}}, Count) ->
            {reply, {ok, #{action => cancel}}, {break, Count + 1}};
        (Entry, _) ->
            ct:fail({unexpected_consumer_entry, Entry})
    end,
    {1, Hnd2} = coderlx:consume(Consumer, 0, Hnd1),
    {{value, #{id := 1, result := #{<<"structuredContent">> := #{
        <<"id">> := 0
      , <<"result">> := #{<<"action">> := <<"cancel">>}
    }}}}, _} =
        coderlx:pop_message(fun(_) -> true end, 1000, Hnd2),
    ok.

-spec response_deadline(proplists:proplist()) -> ok.
response_deadline(Config) ->
    Hnd0 = (?config(coderlx, Config))#{request_timeout := 70},
    Start = erlang:monotonic_time(millisecond),
    Hnd1 = timed_out(fun() ->
        coderlx_mcp:server_tool_call(tool_params(<<"deadline">>), Hnd0)
    end, 1),
    Elapsed = erlang:monotonic_time(millisecond) - Start,
    ?assert(Elapsed >= 70),
    ?assert(Elapsed < 1000),
    ?assertEqual(2, maps:get(next_id, Hnd1)).

-spec expired_handler_preserves_order(proplists:proplist()) -> ok.
expired_handler_preserves_order(Config) ->
    Pending = [
        #{method => 'fs/changed', params => #{watchId => <<"first">>, changedPaths => []}}
      , #{id => 0, method => 'attestation/generate', params => #{}}
      , #{method => 'fs/changed', params => #{watchId => <<"last">>, changedPaths => []}}
    ],
    Hnd0 = (?config(coderlx, Config))#{
        pending := Pending
      , request_timeout := 0
      , server_request_handler => fun(_) -> ct:fail(handler_called_after_deadline) end
    },
    Params = #{clientInfo => #{name => <<"transport-test">>, version => <<"0.1.0">>}},
    Hnd1 = timed_out(fun() -> coderlx:initialize(Params, Hnd0) end, 1),
    ?assertEqual(Pending, maps:get(pending, Hnd1)).

-spec timed_out(
        fun(() -> {coderlx:jsonrpc_error_or(jsone:json_value()), coderlx:coderlx()})
      , pos_integer()
    ) -> coderlx:coderlx().
timed_out(Call, Id) ->
    try Call(), ct:fail(expected_response_timeout)
    catch error:{response_timeout, Id, Hnd} -> Hnd
    end.

-spec tool_params(binary()) -> klsn:rule(coderlx_app_server_rules, mcp_server_tool_call_params).
tool_params(Tool) ->
    #{threadId => <<"thread-1">>, server => <<"fixture">>, tool => Tool, arguments => #{}}.

-spec stop_preserves_other_errors(proplists:proplist()) -> ok.
stop_preserves_other_errors(_Config) ->
    ?assertError(badarg, coderlx:stop(#{stream => #{}})).

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

-spec invalid_handler_option(proplists:proplist()) -> ok.
invalid_handler_option(_Config) ->
    %% An invalid sandbox would fail differently if start reached process creation.
    lists:foreach(fun(Handler) ->
        ?assertError(badarg, coderlx:start(#{
            bwrap => [{invalid_sandbox_option, <<"must not start">>}]
          , server_request_handler => Handler
        }))
    end, [none, undefined, 42, fun() -> ok end, fun(_, _) -> ok end]).

-spec mcp_success_encoding_failure(proplists:proplist()) -> ok.
mcp_success_encoding_failure(Config) ->
    assert_encoding_failure(Config).

-spec mcp_error_encoding_failure(proplists:proplist()) -> ok.
mcp_error_encoding_failure(Config) ->
    assert_encoding_failure(Config).

-spec assert_encoding_failure(proplists:proplist()) -> ok.
assert_encoding_failure(Config) ->
    Hnd0 = ?config(coderlx, Config),
    Hnd1 = try
        coderlx_mcp:server_tool_call(tool_params(<<"elicitation">>), Hnd0),
        ct:fail(expected_encoding_failure)
    catch
        error:{server_request_handler_failed, 0, error, _Reason, Recovered} ->
            Recovered
    end,
    ?assertEqual(2, maps:get(next_id, Hnd1)),
    {{value, #{id := 1, result := #{<<"structuredContent">> :=
        #{<<"error">> := #{<<"code">> := -32000}}}}}, _} =
        coderlx:pop_message(fun
            (#{id := 1, result := _}) -> true;
            (_) -> false
        end, 1000, Hnd1),
    ok.

-spec fixture_dependency_under_tmp(proplists:proplist()) -> ok.
fixture_dependency_under_tmp(Config) ->
    Name = <<"coderlx-jsone-", (list_to_binary(os:getpid()))/binary, "-",
        (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    TempEbin = filename:join(<<"/tmp">>, Name),
    ok = file:make_dir(TempEbin),
    try
        SourceEbin = filename:join(
            unicode:characters_to_binary(code:lib_dir(jsone))
          , <<"ebin">>
        ),
        {ok, Files} = file:list_dir(SourceEbin),
        lists:foreach(fun(File) ->
            {ok, _Bytes} = file:copy(
                filename:join(SourceEbin, File)
              , filename:join(TempEbin, File)
            )
        end, Files),
        %% The sandbox's /tmp hides this host directory unless explicitly bound.
        FixtureConfig = init_per_testcase(mcp_elicitation, [{json_ebin, TempEbin} | Config]),
        try
            mcp_elicitation(FixtureConfig)
        after
            end_per_testcase(mcp_elicitation, FixtureConfig)
        end
    after
        ok = file:del_dir_r(TempEbin)
    end.
