-module(coderlx_SUITE).

-include_lib("common_test/include/ct.hrl").

-define(TEST_MODEL, <<"gpt-5.6-luna">>).
-define(TEST_REASONING_EFFORT, <<"low">>).

-export([all/0]).
-export([
    sandbox_read_test/1,
    sandbox_search_test/1,
    sandbox_edit_test/1,
    sandbox_command_test/1,
    sandbox_isolation_test/1,
    simple_response_test/1
]).

all() ->
    [
        sandbox_read_test,
        sandbox_search_test,
        sandbox_edit_test,
        sandbox_command_test,
        sandbox_isolation_test,
        simple_response_test
    ].

sandbox_read_test(Config) ->
    Workspace = test_workspace(Config, "sandbox-read"),
    ReadContent = <<"read-token-4f87c2">>,
    ok = file:write_file(
        filename:join(Workspace, "read-target.txt"),
        ReadContent
    ),
    Prompt = fail_fast_prompt(
        <<"Read read-target.txt and reply with its exact contents only, "
          "without quotes or formatting.">>
    ),
    #{
        command_completed := true,
        file_change_completed := false,
        operation_failed := false,
        final_text := ReadContent,
        turn := #{error := null}
    } = run_sandbox_turn(Workspace, Prompt),
    ok.

sandbox_search_test(Config) ->
    Workspace = test_workspace(Config, "sandbox-search"),
    TargetPath = filename:join([
        Workspace, "notes", "nested", "search-target.txt"
    ]),
    ok = filelib:ensure_dir(TargetPath),
    ok = file:write_file(TargetPath, <<"sandbox_search_marker_4f87c2\n">>),
    Prompt = fail_fast_prompt(
        <<"Search the current working directory recursively for the file "
          "containing the exact text sandbox_search_marker_4f87c2. Reply "
          "with its path relative to the current working directory, without "
          "a leading `./`, quotes, or formatting.">>
    ),
    #{
        command_completed := true,
        file_change_completed := false,
        operation_failed := false,
        final_text := <<"notes/nested/search-target.txt">>,
        turn := #{error := null}
    } = run_sandbox_turn(Workspace, Prompt),
    ok.

sandbox_edit_test(Config) ->
    Workspace = test_workspace(Config, "sandbox-edit"),
    TargetPath = filename:join(Workspace, "edit-target.txt"),
    InitialContent =
        <<"[sandbox]\n"
          "mode=testing\n"
          "timeout=30\n"
          "\n"
          "[features]\n"
          "read=true\n"
          "write=false\n"
          "command=true\n">>,
    ExpectedContent =
        <<"[sandbox]\n"
          "mode=testing\n"
          "timeout=30\n"
          "\n"
          "[features]\n"
          "read=true\n"
          "search=true\n"
          "write=true\n"
          "command=true\n">>,
    ok = file:write_file(TargetPath, InitialContent),
    Prompt = fail_fast_prompt(
        <<"Edit the `[features]` section of edit-target.txt: insert "
          "`search=true` immediately after `read=true`, and change "
          "`write=false` to `write=true`. Preserve all other content and "
          "ordering. When the edit succeeds, reply exactly "
          "`SANDBOX_EDIT_DONE`.">>
    ),
    #{
        file_change_completed := true,
        operation_failed := false,
        final_text := <<"SANDBOX_EDIT_DONE">>,
        turn := #{error := null}
    } = run_sandbox_turn(Workspace, Prompt),
    {ok, ExpectedContent} = file:read_file(TargetPath),
    ok.

sandbox_command_test(Config) ->
    Workspace = test_workspace(Config, "sandbox-command"),
    ScriptContent =
        <<"#!/bin/sh\n"
          "set -eu\n"
          "printf sandbox-command-ok > command-result.txt\n"
          "printf sandbox-command-complete\n">>,
    ScriptPath = filename:join(Workspace, "write-marker.sh"),
    ok = file:write_file(ScriptPath, ScriptContent),
    Prompt = fail_fast_prompt(
        <<"Run the repository command `/bin/sh ./write-marker.sh`. If its "
          "stdout is `sandbox-command-complete`, reply exactly "
          "`SANDBOX_COMMAND_DONE`.">>
    ),
    #{
        command_completed := true,
        file_change_completed := false,
        operation_failed := false,
        final_text := <<"SANDBOX_COMMAND_DONE">>,
        turn := #{error := null}
    } = run_sandbox_turn(Workspace, Prompt),
    {ok, ScriptContent} = file:read_file(ScriptPath),
    {ok, <<"sandbox-command-ok">>} = file:read_file(
        filename:join(Workspace, "command-result.txt")
    ),
    ok.

sandbox_isolation_test(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Workspace = test_workspace(Config, "sandbox-isolation"),
    OutsideFile = filename:join(PrivDir, "outside-secret.txt"),
    OutsideContent = <<"outside-secret">>,
    OutsidePathContent = unicode:characters_to_binary(OutsideFile),
    ScriptContent =
        <<"#!/bin/sh\n"
          "set -eu\n"
          "outside_path=\"$(cat outside-path.txt)\"\n"
          "if /bin/cat \"$outside_path\" >/dev/null 2>&1; then\n"
          "    printf outside-readable > isolation.txt\n"
          "    exit 86\n"
          "fi\n"
          "if /bin/sh -c 'printf overwritten > \"$1\"' sh \"$outside_path\" "
          "2>/dev/null; then\n"
          "    printf outside-writable > isolation.txt\n"
          "    exit 87\n"
          "fi\n"
          "printf outside-blocked > isolation.txt\n"
          "printf sandbox-isolation-complete\n">>,
    ScriptPath = filename:join(Workspace, "verify-isolation.sh"),
    ok = file:write_file(OutsideFile, OutsideContent),
    ok = file:write_file(
        filename:join(Workspace, "outside-path.txt"),
        OutsidePathContent
    ),
    ok = file:write_file(ScriptPath, ScriptContent),
    Prompt = fail_fast_prompt(
        <<"Run the repository command `/bin/sh ./verify-isolation.sh`. If "
          "its stdout is `sandbox-isolation-complete`, reply exactly "
          "`SANDBOX_ISOLATION_DONE`.">>
    ),
    #{
        command_completed := true,
        file_change_completed := false,
        operation_failed := false,
        final_text := <<"SANDBOX_ISOLATION_DONE">>,
        turn := #{error := null}
    } = run_sandbox_turn(Workspace, Prompt),
    {ok, ScriptContent} = file:read_file(ScriptPath),
    {ok, OutsidePathContent} = file:read_file(
        filename:join(Workspace, "outside-path.txt")
    ),
    {ok, <<"outside-blocked">>} = file:read_file(
        filename:join(Workspace, "isolation.txt")
    ),
    {ok, OutsideContent} = file:read_file(OutsideFile),
    ok.

test_workspace(Config, Name) ->
    Workspace = filename:join(?config(priv_dir, Config), Name),
    ok = filelib:ensure_dir(filename:join(Workspace, ".keep")),
    Workspace.

%% A fallback could hide that the operation Codex normally chose is broken.
fail_fast_prompt(Task) ->
    <<Task/binary,
      " If any command or tool call fails for any reason, stop immediately "
      "and reply exactly `SANDBOX_TEST_FAILED`. Do not retry the failed "
      "operation and do not use a different command, tool, or method. This "
      "failure rule takes priority over completing the task.">>.

run_sandbox_turn(Workspace, Prompt) ->
    Opts = coderlx_opts_builder:sandbox(#{
        sandbox_work_bind => unicode:characters_to_binary(Workspace)
    }),
    {chdir, SandboxWorkspace} = lists:keyfind(chdir, 1, maps:get(bwrap, Opts)),
    Hnd0 = coderlx:start(Opts),
    try
        {{ok, _}, Hnd1} = coderlx:initialize(
            #{clientInfo => #{name => coderlx, version => <<"0.1.0">>}},
            Hnd0
        ),
        {{ok, #{
            thread := #{id := ThreadId},
            model := ?TEST_MODEL
        }}, Hnd2} = coderlx_thread:start(
            #{
                cwd => SandboxWorkspace,
                ephemeral => true,
                model => ?TEST_MODEL
            },
            Hnd1
        ),
        TurnStartParams = #{
            threadId => ThreadId,
            input => [#{type => text, text => Prompt}],
            approvalPolicy => never,
            sandboxPolicy => #{type => externalSandbox},
            model => ?TEST_MODEL,
            effort => ?TEST_REASONING_EFFORT
        },
        {{ok, #{turn := #{error := null, id := TurnId}}}, Hnd3} =
            coderlx_turn:start(TurnStartParams, Hnd2),
        {Result, _Hnd4} = consume_sandbox_turn(ThreadId, TurnId, Hnd3),
        Result
    after
        coderlx:stop(Hnd0)
    end.

consume_sandbox_turn(ThreadId, TurnId, Hnd) ->
    coderlx:consume(fun
        (timeout, _Acc) ->
            error(coderlx_consume_timeout);
        %% Preserve an earlier failure even if the model later finds a fallback.
        ({notification, #{
            method := 'item/completed',
            params := #{
                threadId := ThreadId0,
                turnId := TurnId0,
                item := Item = #{type := commandExecution, status := completed}
            }
        }}, Acc) when ThreadId0 =:= ThreadId, TurnId0 =:= TurnId ->
            Failed = case maps:get(exitCode, Item, 0) of
                ExitCode when is_integer(ExitCode), ExitCode =/= 0 -> true;
                _ -> false
            end,
            {noreply, {continue, Acc#{
                command_completed := true,
                operation_failed := maps:get(operation_failed, Acc) orelse Failed
            }}};
        ({notification, #{
            method := 'item/completed',
            params := #{
                threadId := ThreadId0,
                turnId := TurnId0,
                item := #{type := commandExecution, status := Status}
            }
        }}, Acc) when
            ThreadId0 =:= ThreadId,
            TurnId0 =:= TurnId,
            (Status =:= failed orelse Status =:= declined)
        ->
            {noreply, {continue, Acc#{operation_failed := true}}};
        ({notification, #{
            method := 'item/completed',
            params := #{
                threadId := ThreadId0,
                turnId := TurnId0,
                item := #{type := fileChange, status := completed}
            }
        }}, Acc) when ThreadId0 =:= ThreadId, TurnId0 =:= TurnId ->
            {noreply, {continue, Acc#{file_change_completed := true}}};
        ({notification, #{
            method := 'item/completed',
            params := #{
                threadId := ThreadId0,
                turnId := TurnId0,
                item := #{type := fileChange, status := Status}
            }
        }}, Acc) when
            ThreadId0 =:= ThreadId,
            TurnId0 =:= TurnId,
            (Status =:= failed orelse Status =:= declined)
        ->
            {noreply, {continue, Acc#{operation_failed := true}}};
        ({notification, #{
            method := 'item/completed',
            params := #{
                threadId := ThreadId0,
                turnId := TurnId0,
                item := #{type := agentMessage, text := Text}
            }
        }}, Acc) when ThreadId0 =:= ThreadId, TurnId0 =:= TurnId ->
            {noreply, {continue, Acc#{final_text := Text}}};
        ({notification, #{
            method := 'turn/completed',
            params := #{threadId := ThreadId0, turn := Turn = #{id := TurnId0}}
        }}, Acc) when ThreadId0 =:= ThreadId, TurnId0 =:= TurnId ->
            {noreply, {break, Acc#{turn => Turn}}};
        (_, Acc) ->
            {noreply, {continue, Acc}}
    end, #{
        command_completed => false,
        file_change_completed => false,
        operation_failed => false,
        final_text => <<>>
    }, Hnd).

simple_response_test(_Config) ->
    Hnd0 = coderlx:start(coderlx_opts_builder:sandbox(#{})),
    {{ok, _}, Hnd1} = coderlx:initialize(#{clientInfo => #{name => coderlx, version => <<"0.1.0">>}}, Hnd0),
    {{ok, #{thread := #{id := ThreadId}, model := ?TEST_MODEL}}, Hnd2} =
        coderlx_thread:start(#{model => ?TEST_MODEL}, Hnd1),
    TurnStartParams = #{
            threadId => ThreadId,
            input => [#{type => text, text => <<"Say this is a test.">>}],
            model => ?TEST_MODEL,
            effort => ?TEST_REASONING_EFFORT
        },
    {{ok, #{turn := #{error := null, id := TurnId}}}, Hnd3} = coderlx_turn:start(TurnStartParams, Hnd2),
    {Res, Hnd4} = coderlx:consume(fun
        (timeout, _Acc) ->
            error(coderlx_consume_timeout);
        ({notification, #{method := 'item/completed', params := #{threadId := ThreadId0, turnId := TurnId0, item := #{type := agentMessage, text := Text}}}}, Acc) when ThreadId0 =:= ThreadId, TurnId0 =:= TurnId ->
            {noreply, {continue, <<Acc/binary, Text/binary>>}};
        ({notification, #{method := 'turn/completed', params := #{threadId := ThreadId0, turn := #{id := TurnId0}}}}, Acc) when ThreadId0 =:= ThreadId, TurnId0 =:= TurnId ->
            {noreply, {break, Acc}};
        (_, Acc) ->
            {noreply, {continue, Acc}}
    end, <<>>, Hnd3),
    coderlx:stop(Hnd4),
    assert_expected_result(<<"This is a test">>, Res),
    ok.

assert_expected_result(Expected, Result) ->
    case binary:match(string:lowercase(Result), string:lowercase(Expected)) of
        nomatch ->
             erlang:error(nomatch, [Expected, Result]);
        _ ->
            ok
    end.
