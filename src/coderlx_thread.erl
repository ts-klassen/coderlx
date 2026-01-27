-module(coderlx_thread).
-export([run/2]).
-export_type([thread/0]).

-type event() :: jsone:json_value().
-export_type([event/0]).

-type turn() :: #{
    items := [item()],
    final_response := unicode:unicode_binary(),
    usage := usage()
}.
-type item() :: jsone:json_value().
-type usage() :: jsone:json_value().

-export_type([turn/0, item/0, usage/0]).

-type thread() :: #{
    opts := coderlx:thread_opts(),
    thread_id => unicode:unicode_binary()
}.

-spec run(unicode:unicode_binary(), thread()) -> {turn(), thread()}.
run(Prompt, #{opts := Opts} = Thread) ->
    ThreadIdOpt = klsn_map:lookup([thread_id], Thread),
    Result = klsn_bwrap:run(build_command(Opts, ThreadIdOpt), #{
        bwrap => maps:get(bwrap, Opts),
        timeout => maps:get(timeout, Opts, infinity),
        stdin => <<Prompt/binary, $\n>>
    }),

    collect_turn(result_events(Result), Thread, [], <<>>, #{}).

build_command(Opts, ThreadIdOpt) ->
    CodexPath = maps:get(codex_path, Opts),
    CodexArgs = build_codex_args(Opts, ThreadIdOpt),
    [CodexPath | CodexArgs].

build_codex_args(Opts, ThreadIdOpt) ->
    Args0 = [<<"exec">>, <<"--experimental-json">>, <<"--dangerously-bypass-approvals-and-sandbox">>],
    Args1 = case klsn_map:lookup([working_directory], Opts) of
        none -> Args0;
        {value, WorkDir} -> Args0 ++ [<<"--cd">>, WorkDir]
    end,
    Args2 = case klsn_map:lookup([model], Opts) of
        none -> Args1;
        {value, Model} -> Args1 ++ [<<"--model">>, Model]
    end,
    case ThreadIdOpt of
        none -> Args2;
        {value, ThreadId} -> Args2 ++ [<<"resume">>, ThreadId]
    end.

result_events(#{exit_code := ExitCode, stdout := Stdout, stderr := Stderr}) ->
    case ExitCode of
        0 -> stdout_events(Stdout);
        _ -> erlang:error({codex_exit, #{exit_code => ExitCode, stderr => Stderr, stdout => Stdout}})
    end.

stdout_events(Stdout) when is_binary(Stdout) ->
    Lines = binary:split(Stdout, <<"\n">>, [global]),
    [jsone:decode(Line) || Line <- Lines, Line =/= <<>>].

collect_turn([], _Thread, _Items, _FinalResponse, _Usage) ->
    erlang:error(turn_incomplete);
collect_turn([Event | Rest], Thread, Items, FinalResponse, _Usage) ->
    case maps:get(<<"type">>, Event) of
        <<"thread.started">> ->
            %% Capture thread_id
            NewThreadId = maps:get(<<"thread_id">>, Event),
            Thread1 = Thread#{thread_id => NewThreadId},
            collect_turn(Rest, Thread1, Items, FinalResponse, #{});
        <<"turn.completed">> ->
            Turn = #{
                items => lists:reverse(Items),
                final_response => FinalResponse,
                usage => maps:get(<<"usage">>, Event)
            },
            {Turn, Thread};
        <<"turn.failed">> ->
            erlang:error(maps:get(<<"error">>, Event));
        <<"item.completed">> ->
            Item = maps:get(<<"item">>, Event),
            FinalResponse1 = case maps:get(<<"type">>, Item) of
                <<"agent_message">> -> maps:get(<<"text">>, Item);
                _ -> FinalResponse
            end,
            collect_turn(Rest, Thread, [Item | Items], FinalResponse1, #{});
        _ ->
            collect_turn(Rest, Thread, Items, FinalResponse, #{})
    end.
