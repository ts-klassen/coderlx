#!/usr/bin/env bash
set -euo pipefail

CASE="$1"
ROOT="$2"
REPO="$(cd -P "$3/src/.." && pwd)"
JSON_EBIN="$4"
TARGET=x86_64-unknown-linux-musl
CHILDREN=()
cleanup() {
    touch "$ROOT/release-download" "$ROOT/release-publication"
    for child in "${CHILDREN[@]}"; do
        kill "$child" 2>/dev/null || true
        wait "$child" 2>/dev/null || true
    done
}
mkdir -p "$ROOT/scripts/codex" "$ROOT/src" "$ROOT/fixtures" "$ROOT/shims" \
    "$ROOT/_build/default/lib/jsone/ebin"
trap cleanup EXIT
cp -R "$JSON_EBIN/." "$ROOT/_build/default/lib/jsone/ebin/"
for name in codex_package.sh codex_package.escript ensure_codex_binary.sh update_codex_app_server.sh; do
    cp "$REPO/scripts/$name" "$ROOT/scripts/"
done

cat > "$ROOT/scripts/generate_app_server_rules.escript" <<'ERLANG'
#!/usr/bin/env escript
-spec main([string()]) -> ok.
main([_Schema, Out]) ->
    ok = file:write_file(Out, <<"-module(coderlx_app_server_rules).\n-export([test/0]).\n-spec test() -> ok.\ntest() -> ok.\n">>).
ERLANG
cat > "$ROOT/shims/rebar3" <<'SH'
#!/bin/sh
exit 0
SH
cat > "$ROOT/shims/flock" <<'SH'
#!/bin/sh
echo lock >> "$FIXTURE_ROOT/locks"
exec /usr/bin/flock "$@"
SH
cat > "$ROOT/shims/curl" <<'SH'
#!/bin/bash
set -euo pipefail
url= output=
while [ "$#" -gt 0 ]; do
    case "$1" in
        https://*) url="$1" ;;
        -o) shift; output="$1" ;;
    esac
    shift
done
version="${url#*rust-v}"
version="${version%%/*}"
case "$url" in
    */releases/tags/*) name=release.json ;;
    *) name="${url##*/}" ;;
esac
echo "$url" >> "$FIXTURE_ROOT/downloads"
if [ "${HOLD_DOWNLOAD:-0}" = 1 ]; then
    touch "$FIXTURE_ROOT/download-ready"
    while [ ! -e "$FIXTURE_ROOT/release-download" ]; do sleep 0.01; done
fi
cp "$FIXTURE_ROOT/fixtures/$version/$name" "$output"
SH
cat > "$ROOT/shims/mv" <<'SH'
#!/bin/bash
set -euo pipefail
/usr/bin/mv "$@"
args=("$@")
source="${args[${#args[@]}-2]}"
if [ "${HOLD_PUBLICATION:-0}" = 1 ] && [[ "$source" = */runtime ]]; then
    touch "$FIXTURE_ROOT/publication-ready"
    while [ ! -e "$FIXTURE_ROOT/release-publication" ]; do sleep 0.01; done
fi
SH
chmod +x "$ROOT/shims/"*
export FIXTURE_ROOT="$ROOT"
export PATH="$ROOT/shims:$PATH"

for version in 1.0.0 2.0.0; do
    directory="$ROOT/fixtures/$version"
    runtime="$directory/runtime"
    for name in bin/codex-app-server bin/codex-code-mode-host codex-path/rg \
        codex-resources/bwrap; do
        mkdir -p "$(dirname "$runtime/$name")"
        printf '#!/bin/sh\necho %s\n' "$version" > "$runtime/$name"
        chmod +x "$runtime/$name"
    done
    cat > "$runtime/codex-package.json" <<JSON
{"layoutVersion":1,"version":"$version","target":"$TARGET","variant":"codex-app-server","entrypoint":"bin/codex-app-server","resourcesDir":"codex-resources","pathDir":"codex-path"}
JSON
    escript "$ROOT/scripts/codex_package.escript" manifest "$runtime" "$version" "$directory/manifest"
    printf '#!/bin/sh\nif [ "$1" = "--version" ]; then echo "codex-cli %s"; fi\n' \
        "$version" > "$directory/codex-$TARGET"
    chmod +x "$directory/codex-$TARGET"
    tar -czf "$directory/codex-app-server-package-$TARGET.tar.gz" -C "$runtime" .
    tar -czf "$directory/codex-$TARGET.tar.gz" -C "$directory" "codex-$TARGET"
    package_sha="$(sha256sum "$directory/codex-app-server-package-$TARGET.tar.gz" | cut -d ' ' -f 1)"
    cli_sha="$(sha256sum "$directory/codex-$TARGET.tar.gz" | cut -d ' ' -f 1)"
    cat > "$directory/release.json" <<JSON
{"tag_name":"rust-v$version","prerelease":false,"draft":false,"assets":[{"name":"codex-app-server-package-$TARGET.tar.gz","digest":"sha256:$package_sha"},{"name":"codex-$TARGET.tar.gz","digest":"sha256:$cli_sha"}]}
JSON
    printf 'offline fixture\n' > "$directory/LICENSE"
    printf 'offline fixture\n' > "$directory/NOTICE"
done
printf '1.0.0\n' > "$ROOT/CODEX_VERSION"
cp "$ROOT/fixtures/1.0.0/manifest" "$ROOT/scripts/codex/CODEX_RUNTIME_FILES_SHA256"
sha256sum "$ROOT/fixtures/1.0.0/codex-app-server-package-$TARGET.tar.gz" |
    cut -d ' ' -f 1 > "$ROOT/scripts/codex/CODEX_RUNTIME_TARBALL_SHA256"

# Synchronize on observed events; timeout bounds stalled fixture children.
wait_for_file() {
    until [ -e "$1" ]; do sleep 0.01; done
}
wait_for_locks() {
    until [ -f "$ROOT/locks" ] && [ "$(wc -l < "$ROOT/locks")" -eq 2 ]; do sleep 0.01; done
}

case "$CASE" in
    concurrent_installers)
        HOLD_DOWNLOAD=1 bash "$ROOT/scripts/ensure_codex_binary.sh" &
        first=$!
        CHILDREN+=("$first")
        wait_for_file "$ROOT/download-ready"
        bash "$ROOT/scripts/ensure_codex_binary.sh" &
        second=$!
        CHILDREN+=("$second")
        wait_for_locks
        kill -0 "$second"
        touch "$ROOT/release-download"
        wait "$first"
        wait "$second"
        test "$(wc -l < "$ROOT/downloads")" -eq 1
        escript "$ROOT/scripts/codex_package.escript" verify "$ROOT/priv/codex-runtime" \
            1.0.0 "$ROOT/scripts/codex/CODEX_RUNTIME_FILES_SHA256"
        ;;
    updater_publication)
        mkdir -p "$ROOT/priv"
        cp -R "$ROOT/fixtures/1.0.0/runtime" "$ROOT/priv/codex-runtime"
        HOLD_PUBLICATION=1 bash "$ROOT/scripts/update_codex_app_server.sh" 2.0.0 &
        updater=$!
        CHILDREN+=("$updater")
        wait_for_file "$ROOT/publication-ready"
        bash "$ROOT/scripts/ensure_codex_binary.sh" &
        installer=$!
        CHILDREN+=("$installer")
        wait_for_locks
        kill -0 "$installer"
        touch "$ROOT/release-publication"
        wait "$updater"
        wait "$installer"
        test "$(cat "$ROOT/CODEX_VERSION")" = 2.0.0
        if grep -q rust-v1.0.0 "$ROOT/downloads"; then exit 1; fi
        escript "$ROOT/scripts/codex_package.escript" verify "$ROOT/priv/codex-runtime" \
            2.0.0 "$ROOT/scripts/codex/CODEX_RUNTIME_FILES_SHA256"
        ;;
    failed_rename|conflicting_directory)
        stage="$ROOT/priv/.codex-install.fixture"
        mkdir -p "$ROOT/priv/codex-runtime" "$stage/runtime"
        touch "$ROOT/priv/codex-runtime/old" "$stage/runtime/new"
        cat > "$ROOT/rollback.sh" <<'SH'
#!/bin/bash
set -euo pipefail
ROOT_DIR="$1"
source "$ROOT_DIR/scripts/codex_package.sh"
TMP_DIR="$ROOT_DIR/priv/.codex-install.fixture"
trap 'cleanup_runtime_staging "$TMP_DIR"' EXIT
mv() {
    if [ "$3" = "$TMP_DIR/runtime" ]; then
        if [ "$2" = -- ] && [ "$COLLISION" = 1 ]; then
            mkdir "$RUNTIME_DIR"
            touch "$RUNTIME_DIR/other"
            command mv "$@"
        else
            return 1
        fi
    else
        command mv "$@"
    fi
}
install_runtime "$TMP_DIR/runtime" "$TMP_DIR/previous-runtime"
SH
        COLLISION=0
        if [ "$CASE" = conflicting_directory ]; then COLLISION=1; fi
        export COLLISION
        if bash "$ROOT/rollback.sh" "$ROOT"; then exit 1; fi
        if [ "$CASE" = failed_rename ]; then
            test -f "$ROOT/priv/codex-runtime/old"
            test ! -e "$stage"
        else
            test -f "$ROOT/priv/codex-runtime/other"
            test ! -e "$ROOT/priv/codex-runtime/runtime"
            test -f "$stage/previous-runtime/old"
        fi
        ;;
    *) exit 1 ;;
esac
CHILDREN=()
