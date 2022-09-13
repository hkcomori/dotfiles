#!/usr/bin/env -S bash -e -o pipefail

umask 022
export DEBIAN_FRONTEND=noninteractive

sudo apt-get install -y \
    cloc \
    jq
