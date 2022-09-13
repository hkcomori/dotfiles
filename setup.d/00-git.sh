#!/usr/bin/env -S bash -e -o pipefail

. "$(dirname "$0")"/lib.sh

umask 022
export DEBIAN_FRONTEND=noninteractive

ensure_link \
    .gitconfig \
    repos/.gitconfig

ensure_copy \
    work/.gitkeep

sudo apt-get install -y \
    git
