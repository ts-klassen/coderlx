-module(coderlx_package_checkout_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test callbacks.
-export([suite/0, all/0]).
-export([checkout_only/1, checkout_over_stale_lib/1, removed_checkout/1]).

-type test_case() :: checkout_only | checkout_over_stale_lib | removed_checkout.

-spec suite() -> [{timetrap, {seconds, pos_integer()}}].
suite() ->
    [{timetrap, {seconds, 35}}].

-spec all() -> [test_case()].
all() ->
    [checkout_only, checkout_over_stale_lib, removed_checkout].

-spec checkout_only(proplists:proplist()) -> ok.
checkout_only(Config) ->
    coderlx_package_fixture:run(
        ?config(data_dir, Config)
      , ?config(priv_dir, Config)
      , <<"checkout_only">>).

-spec checkout_over_stale_lib(proplists:proplist()) -> ok.
checkout_over_stale_lib(Config) ->
    coderlx_package_fixture:run(
        ?config(data_dir, Config)
      , ?config(priv_dir, Config)
      , <<"checkout_over_stale_lib">>).

-spec removed_checkout(proplists:proplist()) -> ok.
removed_checkout(Config) ->
    coderlx_package_fixture:run(
        ?config(data_dir, Config)
      , ?config(priv_dir, Config)
      , <<"removed_checkout">>).
