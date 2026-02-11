-module(coderlx_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("klsn/include/klsn_rule_annotation.hrl").

-export([all/0]).
-export([simple_response_test/1]).

%%--------------------------------------------------------------------
%% Test suite setup
%%--------------------------------------------------------------------

-klsn_input_rule([]).
-klsn_output_rule({list, atom}).
-spec all() -> [atom()].
all() ->
    [simple_response_test].

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

-klsn_input_rule([term]).
-klsn_output_rule({exact, ok}).
-spec simple_response_test(term()) -> ok.
simple_response_test(_Config) ->
    BwrapOpts = minimal_sandbox_config(),
    TimeoutMs = 60000,
    CodexPath = codex_path(),

    Conn0 = coderlx:start(#{
        bwrap => BwrapOpts,
        codex_path => CodexPath
    }),

    ClientInfo = #{
        name => <<"coderlx">>,
        title => <<"coderlx">>,
        version => <<"0.1.0">>
    },
    {InitRespOpt, Conn1} = coderlx:initialize(Conn0, ClientInfo, TimeoutMs),
    _InitResp = ensure_success(expect_opt(initialize_response, InitRespOpt)),
    Conn2 = coderlx:initialized(Conn1),

    {ThreadId, Conn3} = start_thread(Conn2, TimeoutMs),

    Prompt = <<"Please respond with exactly: This is a test">>,
    {TurnId, Conn4} = start_turn(Conn3, ThreadId, Prompt, TimeoutMs),
    {FinalResponse, Conn5} = collect_agent_message(Conn4, ThreadId, TurnId, TimeoutMs),
    verify_contains(FinalResponse, <<"This is a test">>),

    Prompt2 = <<"Say again">>,
    {TurnId2, Conn6} = start_turn(Conn5, ThreadId, Prompt2, TimeoutMs),
    {FinalResponse2, Conn7} = collect_agent_message(Conn6, ThreadId, TurnId2, TimeoutMs),
    verify_contains(FinalResponse2, <<"This is a test">>),

    ok = coderlx:stop(Conn7),
    ok.

%%--------------------------------------------------------------------
%% Internal helpers
%%--------------------------------------------------------------------

codex_path() ->
    Path = case os:getenv("CODEX_PATH") of
        false ->
            coderlx:default_codex_path();
        Path0 ->
            klsn_binstr:from_any(Path0)
    end,
    case filelib:is_file(Path) of
        true -> Path;
        false -> ct:fail({codex_binary_not_found, Path})
    end.

start_thread(Conn0, TimeoutMs) ->
    {RequestId, Conn1} = coderlx:send_request(Conn0, 'thread/start', #{}),
    {RespOpt, Conn2} = coderlx:await_response(Conn1, RequestId, TimeoutMs),
    Response = ensure_success(expect_opt(thread_start_response, RespOpt)),
    ThreadId = lookup_required([result, <<"thread">>, <<"id">>], Response),
    {ThreadId, Conn2}.

start_turn(Conn0, ThreadId, Prompt, TimeoutMs) ->
    Params = #{
        threadId => ThreadId,
        input => [#{type => text, text => Prompt}]
    },
    {RequestId, Conn1} = coderlx:send_request(Conn0, 'turn/start', Params),
    {RespOpt, Conn2} = coderlx:await_response(Conn1, RequestId, TimeoutMs),
    Response = ensure_success(expect_opt(turn_start_response, RespOpt)),
    TurnId = lookup_required([result, <<"turn">>, <<"id">>], Response),
    {TurnId, Conn2}.

collect_agent_message(Conn0, ThreadId, TurnId, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    collect_agent_message_loop(Conn0, ThreadId, TurnId, <<>>, Deadline).

collect_agent_message_loop(Conn0, ThreadId, TurnId, Acc, Deadline) ->
    RemainingMs = Deadline - erlang:monotonic_time(millisecond),
    case RemainingMs > 0 of
        true ->
            ok;
        false ->
            erlang:error({turn_timeout, #{thread_id => ThreadId, turn_id => TurnId}})
    end,
    {MessageOpt, Conn1} = coderlx:recv(Conn0, RemainingMs),
    case MessageOpt of
        none ->
            erlang:error({turn_timeout, #{thread_id => ThreadId, turn_id => TurnId}});
        {value, Message} ->
            case message_action(Message, ThreadId, TurnId, Acc) of
                {done, Acc1} ->
                    {Acc1, Conn1};
                {continue, Acc1} ->
                    collect_agent_message_loop(Conn1, ThreadId, TurnId, Acc1, Deadline)
            end
    end.

message_action(Message, ThreadId, TurnId, Acc) ->
    case {maps:is_key(id, Message), klsn_map:lookup([method], Message)} of
        {true, {value, Method}} ->
            ct:fail({unexpected_server_request, Method});
        _ ->
            message_action_method(Message, ThreadId, TurnId, Acc)
    end.

message_action_method(Message, ThreadId, TurnId, Acc) ->
    case klsn_map:lookup([method], Message) of
        {value, 'item/agentMessage/delta'} ->
            case {klsn_map:lookup([params, threadId], Message),
                  klsn_map:lookup([params, turnId], Message),
                  klsn_map:lookup([params, delta], Message)} of
                {{value, ThreadId}, {value, TurnId}, {value, Delta}} ->
                    {continue, <<Acc/binary, Delta/binary>>};
                _ ->
                    {continue, Acc}
            end;
        {value, 'item/completed'} ->
            case {klsn_map:lookup([params, threadId], Message),
                  klsn_map:lookup([params, turnId], Message),
                  klsn_map:lookup([params, item, type], Message),
                  klsn_map:lookup([params, item, text], Message)} of
                {{value, ThreadId}, {value, TurnId}, {value, agentMessage}, {value, Text}} ->
                    {continue, Text};
                _ ->
                    {continue, Acc}
            end;
        {value, 'turn/completed'} ->
            case {klsn_map:lookup([params, threadId], Message),
                  klsn_map:lookup([params, turn, id], Message)} of
                {{value, ThreadId}, {value, TurnId}} ->
                    {done, Acc};
                _ ->
                    {continue, Acc}
            end;
        {value, Method} ->
            case is_codex_event_method(Method) of
                true ->
                    {continue, Acc};
                false ->
                    case Method of
                        error ->
                            ct:fail({server_error_notification, Message});
                        'configWarning' ->
                            ct:fail({server_config_warning, Message});
                        'deprecationNotice' ->
                            ct:fail({server_deprecation_notice, Message});
                        'windows/worldWritableWarning' ->
                            ct:fail({server_world_writable_warning, Message});
                        _ ->
                            case known_notification_method(Method) of
                                true ->
                                    {continue, Acc};
                                false ->
                                    ct:fail({unexpected_notification, Method, Message})
                            end
                    end
            end;
        none ->
            ct:fail({unexpected_message, Message})
    end.

known_notification_method(Method) ->
    lists:member(Method, [
        'account/login/completed',
        'account/rateLimits/updated',
        'account/updated',
        'authStatusChange',
        'item/commandExecution/outputDelta',
        'item/commandExecution/terminalInteraction',
        'item/fileChange/outputDelta',
        'item/mcpToolCall/progress',
        'item/plan/delta',
        'item/reasoning/summaryPartAdded',
        'item/reasoning/summaryTextDelta',
        'item/reasoning/textDelta',
        'item/started',
        'loginChatGptComplete',
        'mcpServer/oauthLogin/completed',
        'rawResponseItem/completed',
        'sessionConfigured',
        'thread/compacted',
        'thread/name/updated',
        'thread/started',
        'thread/tokenUsage/updated',
        'turn/diff/updated',
        'turn/plan/updated',
        'turn/started'
    ]).

is_codex_event_method(Method) when is_atom(Method) ->
    case atom_to_binary(Method, utf8) of
        <<"codex/event/", _/binary>> -> true;
        _ -> false
    end;
is_codex_event_method(_) ->
    false.

ensure_success(Response) ->
    case klsn_map:lookup([error], Response) of
        {value, Error} ->
            erlang:error({jsonrpc_error, Error});
        none ->
            Response
    end.

expect_opt(Tag, Opt) ->
    case Opt of
        {value, Value} ->
            Value;
        none ->
            erlang:error({missing_response, Tag})
    end.

lookup_required(Path, Map) ->
    case klsn_map:lookup(Path, Map) of
        {value, Value} ->
            Value;
        none ->
            erlang:error({missing_key, Path, Map})
    end.

verify_contains(Haystack, Needle) ->
    case binary:match(Haystack, Needle) of
        nomatch ->
            erlang:error({expected_response_to_contain, Needle, Haystack});
        _ ->
            ok
    end.

%% @doc Returns a minimal bwrap configuration that should allow codex to run.
%% This binds the necessary system directories read-only and provides
%% a writable home directory with .codex credentials.
minimal_sandbox_config() ->
    Home = os:getenv("HOME"),
    HomeBin = klsn_binstr:from_any(Home),
    [
        %% Bind entire root filesystem read-only
        {ro_bind, <<"/">>, <<"/">>},
        share_net,

        %% Writable tmp for codex internals
        {tmpfs, <<"/tmp">>},

        %% /proc and /dev
        {proc, <<"/proc">>},
        {dev, <<"/dev">>},

        %% Make .codex writable for credentials/state
        {bind, <<HomeBin/binary, "/.codex">>, <<HomeBin/binary, "/.codex">>},

        %% Set HOME environment variable
        {setenv, <<"HOME">>, HomeBin},

        %% Isolation settings
        die_with_parent
    ].
