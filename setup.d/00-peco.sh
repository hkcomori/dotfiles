#!/usr/bin/env -S bash -e -o pipefail

. "$(dirname "$0")"/lib.sh

umask 022
export DEBIAN_FRONTEND=noninteractive

ensure_link \
    .peco/config.json

sudo apt-get install -y \
    peco
