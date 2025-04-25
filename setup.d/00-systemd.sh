#!/usr/bin/env -S bash -eu -o pipefail

. "$(dirname "$0")"/lib.sh

ensure_link \
    .local/bin/runenv \
    .config/systemd/user/runenv@.service \
    .config/systemd/user/runenv-morning@.timer \
    .config/systemd/user/system-backup.service \
    .config/systemd/user/system-backup.timer \
    .config/systemd/user/user-backup.service \
    .config/systemd/user/user-backup.timer \
    .config/systemd/user/ssh-tunnel@.service

ln -Fs runenv@.service $HOME/.config/systemd/user/runenv-morning@.service

mkdir -p \
    "$HOME/.config/environment.d" \
    "$HOME/.config/runenv.d"
