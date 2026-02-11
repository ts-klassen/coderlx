#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

CODEX_VERSION="${CODEX_VERSION:-0.98.0}"
CODEX_TARGET="${CODEX_TARGET:-x86_64-unknown-linux-musl}"

PRIV_DIR="${ROOT_DIR}/priv"
PRIV_BIN_DIR="${PRIV_DIR}/codex"
CACHE_DIR="${PRIV_BIN_DIR}/cache"
SCHEMA_DIR="${PRIV_BIN_DIR}/schema"

TARBALL_URL="https://github.com/openai/codex/releases/download/rust-v${CODEX_VERSION}/codex-${CODEX_TARGET}.tar.gz"
TARBALL_PATH="${CACHE_DIR}/codex-${CODEX_TARGET}-${CODEX_VERSION}.tar.gz"

mkdir -p "${CACHE_DIR}" "${SCHEMA_DIR}" "${PRIV_BIN_DIR}"

fetch_url() {
    local url="$1"
    local out="$2"

    if ! command -v curl >/dev/null 2>&1; then
        echo "missing curl" >&2
        exit 1
    fi

    curl -fL "${url}" -o "${out}"
}

if [ ! -f "${TARBALL_PATH}" ]; then
    fetch_url "${TARBALL_URL}" "${TARBALL_PATH}"
fi

TMP_DIR="$(mktemp -d "${CACHE_DIR}/tmp.XXXXXX")"
trap 'rm -rf "${TMP_DIR}"' EXIT

tar -xzf "${TARBALL_PATH}" -C "${TMP_DIR}"

if [ ! -f "${TMP_DIR}/codex-${CODEX_TARGET}" ]; then
    echo "codex binary not found after extraction: codex-${CODEX_TARGET}" >&2
    exit 1
fi

mv -f "${TMP_DIR}/codex-${CODEX_TARGET}" "${TMP_DIR}/codex-${CODEX_TARGET}-${CODEX_VERSION}"

chmod +x "${TMP_DIR}/codex-${CODEX_TARGET}-${CODEX_VERSION}"
cp -f "${TMP_DIR}/codex-${CODEX_TARGET}-${CODEX_VERSION}" "${PRIV_BIN_DIR}/"
chmod +x "${PRIV_BIN_DIR}/codex-${CODEX_TARGET}-${CODEX_VERSION}"
ln -sf "codex-${CODEX_TARGET}-${CODEX_VERSION}" "${PRIV_BIN_DIR}/codex"

"${PRIV_BIN_DIR}/codex-${CODEX_TARGET}-${CODEX_VERSION}" app-server generate-json-schema --out "${SCHEMA_DIR}"

ESCRIPT_BIN="/opt/qrpc/pkg/bin/escript"
if [ ! -x "${ESCRIPT_BIN}" ]; then
    echo "missing ${ESCRIPT_BIN}" >&2
    exit 1
fi

rebar3 compile
"${ESCRIPT_BIN}" "${ROOT_DIR}/scripts/generate_app_server_rules.escript" "${SCHEMA_DIR}" "${ROOT_DIR}/src/coderlx_app_server_rules.erl"

fetch_url "https://raw.githubusercontent.com/openai/codex/rust-v${CODEX_VERSION}/LICENSE" "${ROOT_DIR}/LICENSE-CODEX"
fetch_url "https://raw.githubusercontent.com/openai/codex/rust-v${CODEX_VERSION}/NOTICE" "${ROOT_DIR}/NOTICE-CODEX"

echo "codex app-server rules updated for ${CODEX_VERSION} (${CODEX_TARGET})"
