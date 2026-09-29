#!/usr/bin/env bash
# Shared by the installer and updater. ROOT_DIR is set by the caller.

CODEX_TARGET="x86_64-unknown-linux-musl"
CHECKSUM_DIR="${ROOT_DIR}/scripts/codex"
RUNTIME_DIR="${ROOT_DIR}/priv/codex-runtime"
PACKAGE_HELPER="${ROOT_DIR}/scripts/codex_package.escript"

# Linux flock serializes verification and publication, including updated pins.
# Keep the lock file in place: unlinking it could let waiters lock different files.
lock_runtime() {
    mkdir -p "${ROOT_DIR}/priv/.codex-lock"
    exec {CODEX_RUNTIME_LOCK_FD}>"${ROOT_DIR}/priv/.codex-lock/install.lock"
    flock -x "${CODEX_RUNTIME_LOCK_FD}"
}

cleanup_runtime_staging() {
    if [ "${KEEP_RUNTIME_STAGING:-false}" = true ]; then
        echo "preserving runtime recovery files in $1" >&2
    else
        rm -rf -- "$1"
    fi
}

fetch_url() {
    curl -fL --retry 2 --connect-timeout 15 "$1" -o "$2"
}

sha256_file() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$1" | awk '{print $1}'
    else
        shasum -a 256 "$1" | awk '{print $1}'
    fi
}

verify_sha256() {
    local actual
    actual="$(sha256_file "$1")"
    if [ "${actual}" != "$2" ]; then
        echo "sha256 mismatch for $1: expected $2, got ${actual}" >&2
        return 1
    fi
}

read_sha256() {
    local value
    value="$(tr -d '[:space:]' < "$1")"
    if ! [[ "${value}" =~ ^[0-9a-f]{64}$ ]]; then
        echo "invalid sha256 file: $1" >&2
        return 1
    fi
    printf '%s\n' "${value}"
}

validate_version() {
    if ! [[ "$1" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
        echo "expected a stable Codex version, got: $1" >&2
        return 1
    fi
}

# Both directories are under priv, so renames stay on the same filesystem.
# Keep the previous package until the replacement is installed successfully.
install_runtime() {
    local staged="$1" previous="$2"
    if [ -L "${RUNTIME_DIR}" ] || { [ -e "${RUNTIME_DIR}" ] && [ ! -d "${RUNTIME_DIR}" ]; }; then
        echo "expected a package directory: ${RUNTIME_DIR}" >&2
        return 1
    fi
    if [ -e "${previous}" ] || [ -L "${previous}" ]; then
        echo "expected an unused backup path: ${previous}" >&2
        return 1
    fi
    if [ -d "${RUNTIME_DIR}" ]; then
        if ! mv -T -- "${RUNTIME_DIR}" "${previous}"; then
            return 1
        fi
    fi
    if ! mv -T -- "${staged}" "${RUNTIME_DIR}"; then
        if [ -d "${previous}" ]; then
            if ! mv -T -- "${previous}" "${RUNTIME_DIR}"; then
                KEEP_RUNTIME_STAGING=true
                echo "failed to restore previous runtime: ${previous}" >&2
            fi
        fi
        return 1
    fi
}
