#!/usr/bin/env -S bash -l

set -eu

[ "$#" -ne 3 ] && echo "Usage: $(basename "$0") KEY PATH SECRETS" >&2 && exit 1

export key="$1"
export path="$2"
export secrets="$3"

sops -d --extract "${key}" "${secrets}" > "${path}"
