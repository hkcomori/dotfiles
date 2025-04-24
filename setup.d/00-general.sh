#!/usr/bin/env -S bash -eu -o pipefail

. "$(dirname "$0")"/lib.sh

umask 022
export DEBIAN_FRONTEND=noninteractive

sudo apt-get install -y \
    cloc \
    nkf \
    jq

ensure_link \
    .local/bin/reln
