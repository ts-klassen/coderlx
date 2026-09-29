#!/usr/bin/escript
%%! -noshell
-mode(compile).
-include_lib("kernel/include/file.hrl").

-spec main([string()]) -> ok | no_return().
main(Args) ->
    Root = filename:dirname(filename:dirname(escript:script_name())),
    lists:foreach(fun(Kind) ->
        BuiltPaths = filelib:wildcard(filename:join(
            [Root, "_build", "default", Kind, "*", "ebin"])),
        Paths = case Kind of
            "checkouts" ->
                %% Rebar leaves compiled checkout directories after removal.
                lists:filter(fun(Path) ->
                    Name = filename:basename(filename:dirname(Path)),
                    filelib:is_dir(filename:join([Root, "_checkouts", Name]))
                end, BuiltPaths);
            "lib" -> BuiltPaths
        end,
        lists:foreach(fun code:add_pathz/1, Paths)
    end, ["checkouts", "lib"]),
    code:add_pathz(filename:join([filename:dirname(Root), "jsone", "ebin"])),
    try run(Args)
    catch Class:Reason ->
        io:format(standard_error, "codex package: ~p: ~p~n", [Class, Reason]),
        halt(1)
    end.

run(["release-digest", File, Version, Asset]) ->
    Release = read_json(File),
    Tag = list_to_binary("rust-v" ++ Version),
    #{<<"tag_name">> := Tag, <<"prerelease">> := false,
      <<"draft">> := false, <<"assets">> := Assets} = Release,
    Name = list_to_binary(Asset),
    [#{<<"digest">> := <<"sha256:", Digest/binary>>}] =
        [A || A = #{<<"name">> := N} <- Assets, N =:= Name],
    64 = byte_size(Digest),
    true = lists:all(fun(C) ->
        (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f)
    end, binary_to_list(Digest)),
    io:format("~s~n", [Digest]);
run(["manifest", Dir, Version, Out]) ->
    validate_layout(Dir, Version),
    ok = file:write_file(Out, manifest(Dir, Version));
run(["verify", Dir, Version, ExpectedFile]) ->
    %% A dependency's pre-hook can run before jsone is compiled. The pinned
    %% file list includes the version and the validated package JSON's hash,
    %% so installation verification needs only OTP, not a JSON decoder.
    validate_files(Dir),
    {ok, Expected} = file:read_file(ExpectedFile),
    case manifest(Dir, Version) of
        Expected -> ok;
        _ -> error(runtime_checksums_differ)
    end;
run(_) ->
    error("usage: codex_package.escript release-digest <json> <version> <asset> | manifest/verify <dir> <version> <manifest>").

read_json(File) ->
    {ok, Bin} = file:read_file(File),
    jsone:decode(Bin).

validate_layout(Dir, Version) ->
    Vsn = list_to_binary(Version),
    #{<<"layoutVersion">> := 1, <<"version">> := Vsn,
      <<"target">> := <<"x86_64-unknown-linux-musl">>,
      <<"variant">> := <<"codex-app-server">>,
      <<"entrypoint">> := <<"bin/codex-app-server">>,
      <<"resourcesDir">> := <<"codex-resources">>,
      <<"pathDir">> := <<"codex-path">>} =
        read_json(filename:join(Dir, "codex-package.json")),
    validate_files(Dir).

validate_files(Dir) ->
    {ok, #file_info{type = directory}} = file:read_link_info(Dir),
    lists:foreach(fun(Rel) ->
        {ok, #file_info{type = regular, mode = Mode}} =
            file:read_link_info(filename:join(Dir, Rel)),
        case Mode band 8#111 of
            0 -> error({not_executable, Rel});
            _ -> ok
        end
    end, ["bin/codex-app-server", "bin/codex-code-mode-host", "codex-path/rg",
          "codex-resources/bwrap", "codex-resources/zsh/bin/zsh"]).

manifest(Dir, Version) ->
    iolist_to_binary(["# Codex ", Version, " x86_64-unknown-linux-musl\n", [[
        binary:encode_hex(file_hash(filename:join(Dir, Rel)), lowercase),
        "  ", Rel, "\n"
    ] || Rel <- lists:sort(files(Dir, ""))]]).

files(Dir, Prefix) ->
    {ok, Names} = file:list_dir(filename:join(Dir, Prefix)),
    lists:flatmap(fun(Name) ->
        Rel = case Prefix of
            "" -> Name;
            _ -> filename:join(Prefix, Name)
        end,
        {ok, #file_info{type = Type}} = file:read_link_info(filename:join(Dir, Rel)),
        case Type of
            directory -> files(Dir, Rel);
            regular -> [Rel];
            _ -> error({unsupported_package_file, Rel, Type})
        end
    end, Names).

file_hash(File) ->
    {ok, Fd} = file:open(File, [read, binary, raw]),
    try hash_chunks(Fd, crypto:hash_init(sha256))
    after file:close(Fd)
    end.

hash_chunks(Fd, State) ->
    case file:read(Fd, 1024 * 1024) of
        {ok, Bin} -> hash_chunks(Fd, crypto:hash_update(State, Bin));
        eof -> crypto:hash_final(State);
        {error, Reason} -> error({read_package_file, Reason})
    end.
