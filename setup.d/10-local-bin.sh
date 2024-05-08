#!/usr/bin/env -S bash -eu -o pipefail

. "$(dirname "$0")"/lib.sh

umask 022
export DEBIAN_FRONTEND=noninteractive

mkdir -p ~/.local/etc/backup

ensure_link \
    .local/bin/backup_obsidian
