-module(coderlx_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([simple_response_test/1]).

%%--------------------------------------------------------------------
%% Test suite setup
%%--------------------------------------------------------------------

all() ->
    [simple_response_test].

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

%% @doc Test that we can start a thread and get a simple response from codex.
%% This exercises the full stack: bwrap sandboxing, codex process management
%% (via klsn_bwrap), JSON stream protocol parsing, and turn collection.
simple_response_test(Config) ->
    %% Configure bwrap with minimal sandbox that allows codex to run.
    %% Use klsn_bwrap option types directly.
    BwrapOpts = minimal_sandbox_config(),
    TimeoutMs = 60000,

    %% Start a new thread
    Thread0 = coderlx:start_thread(#{
        bwrap => BwrapOpts
      , timeout => TimeoutMs
    }),

    %% Send a prompt asking codex to say something specific
    Prompt = <<"Please respond with exactly: This is a test">>,
    {Turn, Thread1} = coderlx_thread:run(Prompt, Thread0),

    %% Verify we got a response with the expected content
    #{final_response := FinalResponse} = Turn,
    case binary:match(FinalResponse, <<"This is a test">>) of
        nomatch ->
            erlang:error({expected_response_to_contain, <<"This is a test">>, FinalResponse});
        _ ->
            ok
    end,

    %% Second turn - just call run again on the same thread
    Prompt2 = <<"Say again">>,
    {Turn2, _Thread2} = coderlx_thread:run(Prompt2, Thread1),

    #{final_response := FinalResponse2} = Turn2,
    case binary:match(FinalResponse2, <<"This is a test">>) of
        nomatch ->
            erlang:error({expected_response_to_contain, <<"This is a test">>, FinalResponse2});
        _ ->
            ok
    end,

    ok.

%%--------------------------------------------------------------------
%% Internal helpers
%%--------------------------------------------------------------------

%% @doc Returns a minimal bwrap configuration that should allow codex to run.
%% This binds the necessary system directories read-only and provides
%% a writable home directory with .codex credentials.
minimal_sandbox_config() ->
    Home = os:getenv("HOME"),
    HomeBin = list_to_binary(Home),
    [
        %% Bind entire root filesystem read-only
        {ro_bind, <<"/">>, <<"/">>},

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
