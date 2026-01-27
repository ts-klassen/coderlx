-module(coderlx).
-export([start_thread/1]).

-type thread_opts() :: #{
    bwrap := [klsn_bwrap:bwrap_opt()],
    timeout => timeout(),
    codex_path => unicode:unicode_binary(),
    working_directory => unicode:unicode_binary(),
    model => unicode:unicode_binary()
}.
-export_type([thread_opts/0]).

-spec start_thread(thread_opts()) -> coderlx_thread:thread().
start_thread(Opts) ->
    {ok, _} = application:ensure_all_started(coderlx),
    CodexPath = case klsn_map:lookup([codex_path], Opts) of
        {value, CodexPath0} ->
            CodexPath0;
        none ->
            find_codex()
    end,
    #{opts => Opts#{codex_path => CodexPath}}.

find_codex() ->
    case os:find_executable("codex") of
        false -> erlang:error(codex_not_found);
        Path -> list_to_binary(Path)
    end.
