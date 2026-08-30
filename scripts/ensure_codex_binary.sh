#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

CODEX_TARGET="x86_64-unknown-linux-musl"
CODE_MODE_HOST_NAME="codex-code-mode-host"

PRIV_DIR="${ROOT_DIR}/priv"
PRIV_BIN_PATH="${PRIV_DIR}/codex"
PRIV_CODE_MODE_HOST_PATH="${PRIV_DIR}/${CODE_MODE_HOST_NAME}"
CODEX_VERSION_FILE="${ROOT_DIR}/CODEX_VERSION"
CODEX_TARBALL_SHA256_FILE="${ROOT_DIR}/CODEX_TARBALL_SHA256"
CODEX_BINARY_SHA256_FILE="${ROOT_DIR}/CODEX_BINARY_SHA256"
CODE_MODE_HOST_TARBALL_SHA256_FILE="${ROOT_DIR}/CODEX_CODE_MODE_HOST_TARBALL_SHA256"
CODE_MODE_HOST_BINARY_SHA256_FILE="${ROOT_DIR}/CODEX_CODE_MODE_HOST_BINARY_SHA256"

if [ ! -f "${CODEX_VERSION_FILE}" ]; then
    echo "missing ${CODEX_VERSION_FILE}; run scripts/update_codex_app_server.sh <version>" >&2
    exit 1
fi

CODEX_VERSION="$(tr -d '[:space:]' < "${CODEX_VERSION_FILE}")"
if [ -z "${CODEX_VERSION}" ]; then
    echo "empty ${CODEX_VERSION_FILE}; run scripts/update_codex_app_server.sh <version>" >&2
    exit 1
fi

if [ ! -f "${CODEX_TARBALL_SHA256_FILE}" ]; then
    echo "missing ${CODEX_TARBALL_SHA256_FILE}; run scripts/update_codex_app_server.sh <version>" >&2
    exit 1
fi

CODEX_TARBALL_SHA256="$(tr -d '[:space:]' < "${CODEX_TARBALL_SHA256_FILE}" | tr '[:upper:]' '[:lower:]')"
if ! [[ "${CODEX_TARBALL_SHA256}" =~ ^[0-9a-f]{64}$ ]]; then
    echo "invalid ${CODEX_TARBALL_SHA256_FILE}; run scripts/update_codex_app_server.sh <version>" >&2
    exit 1
fi

if [ ! -f "${CODEX_BINARY_SHA256_FILE}" ]; then
    echo "missing ${CODEX_BINARY_SHA256_FILE}; run scripts/update_codex_app_server.sh <version>" >&2
    exit 1
fi

CODEX_BINARY_SHA256="$(tr -d '[:space:]' < "${CODEX_BINARY_SHA256_FILE}" | tr '[:upper:]' '[:lower:]')"
if ! [[ "${CODEX_BINARY_SHA256}" =~ ^[0-9a-f]{64}$ ]]; then
    echo "invalid ${CODEX_BINARY_SHA256_FILE}; run scripts/update_codex_app_server.sh <version>" >&2
    exit 1
fi

if [ ! -f "${CODE_MODE_HOST_TARBALL_SHA256_FILE}" ]; then
    echo "missing ${CODE_MODE_HOST_TARBALL_SHA256_FILE}; run scripts/update_codex_app_server.sh <version>" >&2
    exit 1
fi

CODE_MODE_HOST_TARBALL_SHA256="$(tr -d '[:space:]' < "${CODE_MODE_HOST_TARBALL_SHA256_FILE}" | tr '[:upper:]' '[:lower:]')"
if ! [[ "${CODE_MODE_HOST_TARBALL_SHA256}" =~ ^[0-9a-f]{64}$ ]]; then
    echo "invalid ${CODE_MODE_HOST_TARBALL_SHA256_FILE}; run scripts/update_codex_app_server.sh <version>" >&2
    exit 1
fi

if [ ! -f "${CODE_MODE_HOST_BINARY_SHA256_FILE}" ]; then
    echo "missing ${CODE_MODE_HOST_BINARY_SHA256_FILE}; run scripts/update_codex_app_server.sh <version>" >&2
    exit 1
fi

CODE_MODE_HOST_BINARY_SHA256="$(tr -d '[:space:]' < "${CODE_MODE_HOST_BINARY_SHA256_FILE}" | tr '[:upper:]' '[:lower:]')"
if ! [[ "${CODE_MODE_HOST_BINARY_SHA256}" =~ ^[0-9a-f]{64}$ ]]; then
    echo "invalid ${CODE_MODE_HOST_BINARY_SHA256_FILE}; run scripts/update_codex_app_server.sh <version>" >&2
    exit 1
fi

TARBALL_URL="https://github.com/openai/codex/releases/download/rust-v${CODEX_VERSION}/codex-${CODEX_TARGET}.tar.gz"
CODE_MODE_HOST_TARBALL_URL="https://github.com/openai/codex/releases/download/rust-v${CODEX_VERSION}/${CODE_MODE_HOST_NAME}-${CODEX_TARGET}.tar.gz"

mkdir -p "${PRIV_DIR}"
if [ -d "${PRIV_BIN_PATH}" ]; then
    echo "expected ${PRIV_BIN_PATH} to be a file, but it is a directory; remove ${PRIV_BIN_PATH} to continue" >&2
    exit 1
fi
if [ -d "${PRIV_CODE_MODE_HOST_PATH}" ]; then
    echo "expected ${PRIV_CODE_MODE_HOST_PATH} to be a file, but it is a directory; remove ${PRIV_CODE_MODE_HOST_PATH} to continue" >&2
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

CODEX_CURRENT=false
if [ -f "${PRIV_BIN_PATH}" ]; then
    EXISTING_SHA256="$(sha256_file "${PRIV_BIN_PATH}" | tr '[:upper:]' '[:lower:]')"
    if [ "${EXISTING_SHA256}" = "${CODEX_BINARY_SHA256}" ]; then
        CODEX_CURRENT=true
    else
        echo "codex binary hash mismatch; re-downloading" >&2
    fi
fi

CODE_MODE_HOST_CURRENT=false
if [ -f "${PRIV_CODE_MODE_HOST_PATH}" ]; then
    EXISTING_CODE_MODE_HOST_SHA256="$(sha256_file "${PRIV_CODE_MODE_HOST_PATH}" | tr '[:upper:]' '[:lower:]')"
    if [ "${EXISTING_CODE_MODE_HOST_SHA256}" = "${CODE_MODE_HOST_BINARY_SHA256}" ]; then
        CODE_MODE_HOST_CURRENT=true
    else
        echo "codex code-mode host binary hash mismatch; re-downloading" >&2
    fi
fi

if [ "${CODEX_CURRENT}" = true ] && [ "${CODE_MODE_HOST_CURRENT}" = true ]; then
    exit 0
fi

TMP_ROOT="${TMPDIR:-/tmp}"
TMP_DIR="$(mktemp -d "${TMP_ROOT%/}/coderlx-codex.XXXXXX")"
trap 'rm -rf "${TMP_DIR}"' EXIT

TARBALL_PATH="${TMP_DIR}/codex-${CODEX_TARGET}-${CODEX_VERSION}.tar.gz"
CODE_MODE_HOST_TARBALL_PATH="${TMP_DIR}/${CODE_MODE_HOST_NAME}-${CODEX_TARGET}-${CODEX_VERSION}.tar.gz"
fetch_url "${TARBALL_URL}" "${TARBALL_PATH}"
fetch_url "${CODE_MODE_HOST_TARBALL_URL}" "${CODE_MODE_HOST_TARBALL_PATH}"

ACTUAL_SHA256="$(sha256_file "${TARBALL_PATH}" | tr '[:upper:]' '[:lower:]')"
if [ "${ACTUAL_SHA256}" != "${CODEX_TARBALL_SHA256}" ]; then
    echo "codex tarball sha256 mismatch: expected ${CODEX_TARBALL_SHA256}, got ${ACTUAL_SHA256}" >&2
    exit 1
fi

ACTUAL_CODE_MODE_HOST_SHA256="$(sha256_file "${CODE_MODE_HOST_TARBALL_PATH}" | tr '[:upper:]' '[:lower:]')"
if [ "${ACTUAL_CODE_MODE_HOST_SHA256}" != "${CODE_MODE_HOST_TARBALL_SHA256}" ]; then
    echo "codex code-mode host tarball sha256 mismatch: expected ${CODE_MODE_HOST_TARBALL_SHA256}, got ${ACTUAL_CODE_MODE_HOST_SHA256}" >&2
    exit 1
fi

tar -xzf "${TARBALL_PATH}" -C "${TMP_DIR}"
tar -xzf "${CODE_MODE_HOST_TARBALL_PATH}" -C "${TMP_DIR}"

if [ ! -f "${TMP_DIR}/codex-${CODEX_TARGET}" ]; then
    echo "codex binary not found after extraction: codex-${CODEX_TARGET}" >&2
    exit 1
fi
if [ ! -f "${TMP_DIR}/${CODE_MODE_HOST_NAME}-${CODEX_TARGET}" ]; then
    echo "codex code-mode host binary not found after extraction: ${CODE_MODE_HOST_NAME}-${CODEX_TARGET}" >&2
    exit 1
fi

EXTRACTED_SHA256="$(sha256_file "${TMP_DIR}/codex-${CODEX_TARGET}" | tr '[:upper:]' '[:lower:]')"
if [ "${EXTRACTED_SHA256}" != "${CODEX_BINARY_SHA256}" ]; then
    echo "codex binary sha256 mismatch: expected ${CODEX_BINARY_SHA256}, got ${EXTRACTED_SHA256}" >&2
    exit 1
fi

EXTRACTED_CODE_MODE_HOST_SHA256="$(sha256_file "${TMP_DIR}/${CODE_MODE_HOST_NAME}-${CODEX_TARGET}" | tr '[:upper:]' '[:lower:]')"
if [ "${EXTRACTED_CODE_MODE_HOST_SHA256}" != "${CODE_MODE_HOST_BINARY_SHA256}" ]; then
    echo "codex code-mode host binary sha256 mismatch: expected ${CODE_MODE_HOST_BINARY_SHA256}, got ${EXTRACTED_CODE_MODE_HOST_SHA256}" >&2
    exit 1
fi

chmod +x "${TMP_DIR}/codex-${CODEX_TARGET}"
cp -f "${TMP_DIR}/codex-${CODEX_TARGET}" "${PRIV_BIN_PATH}"
chmod +x "${PRIV_BIN_PATH}"
chmod +x "${TMP_DIR}/${CODE_MODE_HOST_NAME}-${CODEX_TARGET}"
cp -f "${TMP_DIR}/${CODE_MODE_HOST_NAME}-${CODEX_TARGET}" "${PRIV_CODE_MODE_HOST_PATH}"
chmod +x "${PRIV_CODE_MODE_HOST_PATH}"
