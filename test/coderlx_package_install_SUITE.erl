-module(coderlx_package_install_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test callbacks.
-export([suite/0, all/0]).
-export([
        concurrent_installers/1
      , updater_publication/1
      , failed_rename/1
      , conflicting_directory/1
      , unicode_paths/1
    ]).

-type test_case() :: concurrent_installers | updater_publication |
    failed_rename | conflicting_directory | unicode_paths.

-spec suite() -> [{timetrap, {seconds, pos_integer()}}].
suite() ->
    [{timetrap, {seconds, 35}}].

-spec all() -> [test_case()].
all() ->
    [
        concurrent_installers
      , updater_publication
      , failed_rename
      , conflicting_directory
      , unicode_paths
    ].

-spec concurrent_installers(proplists:proplist()) -> ok.
concurrent_installers(Config) ->
    coderlx_package_fixture:run(
        ?config(data_dir, Config)
      , ?config(priv_dir, Config)
      , <<"concurrent_installers">>).

-spec updater_publication(proplists:proplist()) -> ok.
updater_publication(Config) ->
    coderlx_package_fixture:run(
        ?config(data_dir, Config)
      , ?config(priv_dir, Config)
      , <<"updater_publication">>).

-spec failed_rename(proplists:proplist()) -> ok.
failed_rename(Config) ->
    coderlx_package_fixture:run(
        ?config(data_dir, Config)
      , ?config(priv_dir, Config)
      , <<"failed_rename">>).

-spec conflicting_directory(proplists:proplist()) -> ok.
conflicting_directory(Config) ->
    coderlx_package_fixture:run(
        ?config(data_dir, Config)
      , ?config(priv_dir, Config)
      , <<"conflicting_directory">>).

-spec unicode_paths(proplists:proplist()) -> ok.
unicode_paths(Config) ->
    PrivDir = unicode:characters_to_binary(?config(priv_dir, Config)),
    DataDir = filename:join(PrivDir, <<"données"/utf8>>),
    UnicodePrivDir = filename:join(PrivDir, <<"結果"/utf8>>),
    Script = filename:join(DataDir, <<"fixture.sh">>),
    ok = filelib:ensure_dir(Script),
    Source = filename:join(
        unicode:characters_to_binary(?config(data_dir, Config))
      , <<"fixture.sh">>),
    {ok, _} = file:copy(Source, Script),
    coderlx_package_fixture:run(DataDir, UnicodePrivDir, <<"failed_rename">>),
    true = filelib:is_regular(filename:join(
        UnicodePrivDir
      , <<"failed_rename/priv/codex-runtime/old">>)),
    ok.
