#!/usr/bin/env -S bash -eu -o pipefail

. "$(dirname "$0")"/lib.sh

umask 022
export DEBIAN_FRONTEND=noninteractive

ensure_link \
    .gitconfig \
    repos/.gitconfig

ensure_copy \
    work/.gitkeep \
    mnt/.gitkeep

sudo apt-get install -y \
    git
