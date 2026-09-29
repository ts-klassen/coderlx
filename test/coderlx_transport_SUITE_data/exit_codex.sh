#!/bin/sh
set -eu
printf 'ready\n' >&2
IFS= read -r status
printf '%s\n' '{"id":1,"result":{}}' '{"id":2,"result":{}}'
exit "$status"
