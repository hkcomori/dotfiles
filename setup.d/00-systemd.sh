#!/usr/bin/env -S bash -eu -o pipefail

. "$(dirname "$0")"/lib.sh

ensure_link \
    .local/bin/snapshot \
    .config/snapshot.d/windows.env.sh \
    .config/systemd/user/snapshot@.service \
    .config/systemd/user/snapshot-morning@.timer \
    .config/systemd/user/ssh-tunnel@.service

ln -fFs snapshot@.service $HOME/.config/systemd/user/snapshot-morning@.service

mkdir -p \
    "$HOME/.config/environment.d" \
    "$HOME/.config/snapshot.d"
