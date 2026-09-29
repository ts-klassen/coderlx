#!/bin/sh
set -eu
printf 'ready\n' >&2
IFS= read -r request
printf '%s\n' "$request" >&2
printf '{"id":1,"result":{}}\n'
exec /bin/cat >&2
