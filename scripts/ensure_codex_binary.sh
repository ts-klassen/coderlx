#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "${ROOT_DIR}/scripts/codex_package.sh"

lock_runtime
CODEX_VERSION="$(tr -d '[:space:]' < "${ROOT_DIR}/CODEX_VERSION")"
validate_version "${CODEX_VERSION}"
PACKAGE_SHA256="$(read_sha256 "${CHECKSUM_DIR}/CODEX_RUNTIME_TARBALL_SHA256")"
FILES_SHA256="${CHECKSUM_DIR}/CODEX_RUNTIME_FILES_SHA256"
test -s "${FILES_SHA256}"

if [ -d "${RUNTIME_DIR}" ] &&
    escript "${PACKAGE_HELPER}" verify "${RUNTIME_DIR}" "${CODEX_VERSION}" "${FILES_SHA256}"; then
    exit 0
fi

TMP_DIR="$(mktemp -d "${ROOT_DIR}/priv/.codex-install.XXXXXX")"
trap 'cleanup_runtime_staging "${TMP_DIR}"' EXIT
PACKAGE_NAME="codex-app-server-package-${CODEX_TARGET}.tar.gz"
fetch_url "https://github.com/openai/codex/releases/download/rust-v${CODEX_VERSION}/${PACKAGE_NAME}" "${TMP_DIR}/package.tar.gz"
verify_sha256 "${TMP_DIR}/package.tar.gz" "${PACKAGE_SHA256}"
mkdir "${TMP_DIR}/runtime"
tar -xzf "${TMP_DIR}/package.tar.gz" -C "${TMP_DIR}/runtime"
escript "${PACKAGE_HELPER}" verify "${TMP_DIR}/runtime" "${CODEX_VERSION}" "${FILES_SHA256}"
install_runtime "${TMP_DIR}/runtime" "${TMP_DIR}/previous-runtime"
