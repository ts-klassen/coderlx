#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "${ROOT_DIR}/scripts/codex_package.sh"

if [ "$#" -ne 1 ]; then
    echo "usage: $(basename "$0") <codex_version>" >&2
    exit 1
fi
CODEX_VERSION="$1"
validate_version "${CODEX_VERSION}"
cd "${ROOT_DIR}"

# Bootstrap existing dependencies without the app's compile hook or wrappers
# that may already reference the next version's generated aliases.
rebar3 compile --deps_only

mkdir -p "${ROOT_DIR}/priv"
TMP_DIR="$(mktemp -d "${ROOT_DIR}/priv/.codex-update.XXXXXX")"
trap 'rm -rf "${TMP_DIR}"' EXIT
PACKAGE_NAME="codex-app-server-package-${CODEX_TARGET}.tar.gz"
CLI_NAME="codex-${CODEX_TARGET}.tar.gz"
RELEASE_URL="https://github.com/openai/codex/releases/download/rust-v${CODEX_VERSION}"
fetch_url "https://api.github.com/repos/openai/codex/releases/tags/rust-v${CODEX_VERSION}" "${TMP_DIR}/release.json"
PACKAGE_SHA256="$(escript "${PACKAGE_HELPER}" release-digest "${TMP_DIR}/release.json" "${CODEX_VERSION}" "${PACKAGE_NAME}")"
CLI_SHA256="$(escript "${PACKAGE_HELPER}" release-digest "${TMP_DIR}/release.json" "${CODEX_VERSION}" "${CLI_NAME}")"

fetch_url "${RELEASE_URL}/${PACKAGE_NAME}" "${TMP_DIR}/package.tar.gz"
verify_sha256 "${TMP_DIR}/package.tar.gz" "${PACKAGE_SHA256}"
fetch_url "${RELEASE_URL}/${CLI_NAME}" "${TMP_DIR}/cli.tar.gz"
verify_sha256 "${TMP_DIR}/cli.tar.gz" "${CLI_SHA256}"
mkdir "${TMP_DIR}/runtime" "${TMP_DIR}/cli" "${TMP_DIR}/schema"
tar -xzf "${TMP_DIR}/package.tar.gz" -C "${TMP_DIR}/runtime"
tar -xzf "${TMP_DIR}/cli.tar.gz" -C "${TMP_DIR}/cli"
escript "${PACKAGE_HELPER}" manifest "${TMP_DIR}/runtime" "${CODEX_VERSION}" "${TMP_DIR}/CODEX_RUNTIME_FILES_SHA256"

CLI="${TMP_DIR}/cli/codex-${CODEX_TARGET}"
test "$("${CLI}" --version)" = "codex-cli ${CODEX_VERSION}"
"${CLI}" app-server generate-json-schema --out "${TMP_DIR}/schema"
escript "${ROOT_DIR}/scripts/generate_app_server_rules.escript" "${TMP_DIR}/schema" "${TMP_DIR}/coderlx_app_server_rules.erl"
erlc -o "${TMP_DIR}" "${TMP_DIR}/coderlx_app_server_rules.erl"

fetch_url "https://raw.githubusercontent.com/openai/codex/rust-v${CODEX_VERSION}/LICENSE" "${TMP_DIR}/LICENSE-CODEX"
fetch_url "https://raw.githubusercontent.com/openai/codex/rust-v${CODEX_VERSION}/NOTICE" "${TMP_DIR}/NOTICE-CODEX"
printf '%s\n' "${CODEX_VERSION}" > "${TMP_DIR}/CODEX_VERSION"
printf '%s\n' "${PACKAGE_SHA256}" > "${TMP_DIR}/CODEX_RUNTIME_TARBALL_SHA256"
printf '%s\n' "${CLI_SHA256}" > "${TMP_DIR}/CODEX_SCHEMA_CLI_TARBALL_SHA256"

# Validate and compile everything above before replacing pins or binaries.
install_runtime "${TMP_DIR}/runtime" "${TMP_DIR}/previous-runtime"
for name in CODEX_RUNTIME_TARBALL_SHA256 CODEX_RUNTIME_FILES_SHA256 CODEX_SCHEMA_CLI_TARBALL_SHA256; do
    mv "${TMP_DIR}/${name}" "${CHECKSUM_DIR}/${name}"
done
for name in CODEX_VERSION LICENSE-CODEX NOTICE-CODEX; do
    mv "${TMP_DIR}/${name}" "${ROOT_DIR}/${name}"
done
mv "${TMP_DIR}/coderlx_app_server_rules.erl" "${ROOT_DIR}/src/coderlx_app_server_rules.erl"
echo "codex app-server package and rules updated for ${CODEX_VERSION} (${CODEX_TARGET})"
