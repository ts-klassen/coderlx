#!/usr/bin/env bash
set -euo pipefail

CASE="$1"
ROOT="$2"
REPO="$(cd -P "$3/src/.." && pwd)"
JSON_EBIN="$4"
mkdir -p "$ROOT/scripts" "$ROOT/_build/default/checkouts/jsone/ebin" \
    "$ROOT/_checkouts/jsone"
cp "$REPO/scripts/codex_package.escript" "$ROOT/scripts/"
cp -R "$JSON_EBIN/." "$ROOT/_build/default/checkouts/jsone/ebin/"
cp -R "$JSON_EBIN/../." "$ROOT/_checkouts/jsone/"

case "$CASE" in
    checkout_only) ;;
    checkout_over_stale_lib|removed_checkout)
        mkdir -p "$ROOT/_build/default/lib/jsone/ebin"
        if [ "$CASE" = removed_checkout ]; then
            cp -R "$JSON_EBIN/." "$ROOT/_build/default/lib/jsone/ebin/"
            mv "$ROOT/_checkouts/jsone" "$ROOT/removed-jsone"
            test ! -d "$ROOT/_checkouts/jsone"
            STALE_KIND=checkouts
        else
            STALE_KIND=lib
        fi
        cat > "$ROOT/jsone.erl" <<'ERLANG'
-module(jsone).
-export([decode/1]).
-spec decode(binary()) -> no_return().
decode(_Json) -> error(stale_dependency_used).
ERLANG
        erlc -o "$ROOT/_build/default/$STALE_KIND/jsone/ebin" "$ROOT/jsone.erl"
        ;;
    *) exit 1 ;;
esac

DIGEST=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
cat > "$ROOT/release.json" <<JSON
{"tag_name":"rust-v1.0.0","prerelease":false,"draft":false,"assets":[{"name":"fixture.tar.gz","digest":"sha256:$DIGEST"}]}
JSON
ACTUAL="$(escript "$ROOT/scripts/codex_package.escript" release-digest \
    "$ROOT/release.json" 1.0.0 fixture.tar.gz)"
test "$ACTUAL" = "$DIGEST"
