#!/usr/bin/env -S bash -eu -o pipefail

. "$(dirname "$0")"/lib.sh

ensure_link \
    .local/bin/runenv \
    .config/systemd/user/system-backup.service \
    .config/systemd/user/system-backup.timer \
    .config/systemd/user/user-backup.service \
    .config/systemd/user/user-backup.timer \
    .config/systemd/user/ssh-tunnel@.service

mkdir -p \
    "$HOME/.config/environment.d" \
    "$HOME/.config/runenv.d"
