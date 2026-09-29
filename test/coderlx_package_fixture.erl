-module(coderlx_package_fixture).

-export([run/3]).

-spec run(file:filename_all(), file:filename_all(), binary()) -> ok.
run(DataDir, PrivDir, Case) ->
    Script = filename:join(unicode:characters_to_binary(DataDir), <<"fixture.sh">>),
    Root = filename:join(unicode:characters_to_binary(PrivDir), Case),
    AppDir = unicode:characters_to_binary(code:lib_dir(coderlx)),
    JsonEbin = filename:join(unicode:characters_to_binary(code:lib_dir(jsone)), <<"ebin">>),
    %% timeout bounds the whole fixture process group, including paused children.
    Port = open_port({spawn_executable, "/usr/bin/timeout"}, [
        binary
      , exit_status
      , use_stdio
      , stderr_to_stdout
      , {args, ["--kill-after=1s", "25s", "/bin/bash" | lists:map(
            fun unicode:characters_to_list/1, [Script, Case, Root, AppDir, JsonEbin])]}
    ]),
    try
        await_fixture(Port, <<>>)
    after
        %% The port normally closes before exit_status is delivered.
        try port_close(Port)
        catch error:badarg -> ok
        end
    end.

-spec await_fixture(port(), binary()) -> ok.
await_fixture(Port, Output) ->
    receive
        {Port, {data, Data}} ->
            await_fixture(Port, <<Output/binary, Data/binary>>);
        {Port, {exit_status, 0}} ->
            ok;
        {Port, {exit_status, Status}} ->
            ct:fail({package_fixture_failed, Status, Output})
    after 30000 ->
        ct:fail({package_fixture_timeout, Output})
    end.
