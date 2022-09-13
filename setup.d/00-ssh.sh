#!/usr/bin/env -S bash -e -o pipefail

. "$(dirname "$0")"/lib.sh

umask 022
export DEBIAN_FRONTEND=noninteractive

ensure_link \
    .gnupg/gpg-agent.conf \
    .gnupg/gpg.conf \
    .ssh/config

ensure_copy \
    .gnupg/sshcontrol

sudo apt-get install -y \
    gnupg \
    openssh-client
