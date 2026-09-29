#!/bin/sh
set -eu
printf 'ready\n' >&2
IFS= read -r request
printf '%s\n' '{"id":1,"method":"attestation/generate","params":{}}'
printf '%s\n' '{"id":1,"result":{}}'
exec /bin/cat >&2
