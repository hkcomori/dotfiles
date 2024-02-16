#!/usr/bin/env -S bash -eu -o pipefail

. "$(dirname "$0")"/lib.sh

ensure_link \
    .config/systemd/user/ssh-tunnel@.service
