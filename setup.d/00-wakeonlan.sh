#!/usr/bin/env -S bash -eu -o pipefail

. "$(dirname "$0")"/lib.sh

umask 022
export DEBIAN_FRONTEND=noninteractive

sudo apt-get install -y \
    wakeonlan

ensure_link \
    .config/systemd/user/wakeonlan@.service \
    .config/systemd/user/wakeonlan@.timer
