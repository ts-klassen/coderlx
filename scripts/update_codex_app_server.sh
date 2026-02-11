#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

CODEX_TARGET="x86_64-unknown-linux-musl"
CODEX_VERSION_FILE="${ROOT_DIR}/CODEX_VERSION"
CODEX_TARBALL_SHA256_FILE="${ROOT_DIR}/CODEX_TARBALL_SHA256"
CODEX_BINARY_SHA256_FILE="${ROOT_DIR}/CODEX_BINARY_SHA256"

if [ "${#}" -ne 1 ]; then
    echo "usage: $(basename "$0") <codex_version>" >&2
    exit 1
fi

CODEX_VERSION="$1"

PRIV_DIR="${ROOT_DIR}/priv"
PRIV_BIN_PATH="${PRIV_DIR}/codex"

TARBALL_URL="https://github.com/openai/codex/releases/download/rust-v${CODEX_VERSION}/codex-${CODEX_TARGET}.tar.gz"

mkdir -p "${PRIV_DIR}"
if [ -d "${PRIV_BIN_PATH}" ]; then
    echo "expected ${PRIV_BIN_PATH} to be a file, but it is a directory; remove ${PRIV_BIN_PATH} to continue" >&2
    exit 1
fi

fetch_url() {
    local url="$1"
    local out="$2"

    if ! command -v curl >/dev/null 2>&1; then
        echo "missing curl" >&2
        exit 1
    fi

    curl -fL "${url}" -o "${out}"
}

sha256_file() {
    local path="$1"

    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "${path}" | awk '{print $1}'
        return 0
    fi

    if command -v shasum >/dev/null 2>&1; then
        shasum -a 256 "${path}" | awk '{print $1}'
        return 0
    fi

    echo "missing sha256sum or shasum" >&2
    exit 1
}

TMP_ROOT="${TMPDIR:-/tmp}"
TMP_DIR="$(mktemp -d "${TMP_ROOT%/}/coderlx-codex.XXXXXX")"
trap 'rm -rf "${TMP_DIR}"' EXIT

SCHEMA_DIR="${TMP_DIR}/schema"
TARBALL_PATH="${TMP_DIR}/codex-${CODEX_TARGET}-${CODEX_VERSION}.tar.gz"
mkdir -p "${SCHEMA_DIR}"
fetch_url "${TARBALL_URL}" "${TARBALL_PATH}"

CODEX_TARBALL_SHA256="$(sha256_file "${TARBALL_PATH}" | tr '[:upper:]' '[:lower:]')"

tar -xzf "${TARBALL_PATH}" -C "${TMP_DIR}"

if [ ! -f "${TMP_DIR}/codex-${CODEX_TARGET}" ]; then
    echo "codex binary not found after extraction: codex-${CODEX_TARGET}" >&2
    exit 1
fi

CODEX_BINARY_SHA256="$(sha256_file "${TMP_DIR}/codex-${CODEX_TARGET}" | tr '[:upper:]' '[:lower:]')"

printf '%s\n' "${CODEX_VERSION}" > "${CODEX_VERSION_FILE}"
printf '%s\n' "${CODEX_TARBALL_SHA256}" > "${CODEX_TARBALL_SHA256_FILE}"
printf '%s\n' "${CODEX_BINARY_SHA256}" > "${CODEX_BINARY_SHA256_FILE}"

chmod +x "${TMP_DIR}/codex-${CODEX_TARGET}"
cp -f "${TMP_DIR}/codex-${CODEX_TARGET}" "${PRIV_BIN_PATH}"
chmod +x "${PRIV_BIN_PATH}"

"${PRIV_BIN_PATH}" app-server generate-json-schema --out "${SCHEMA_DIR}"

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
