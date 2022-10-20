#!/usr/bin/env -S bash -eu -o pipefail

. "$(dirname "$0")"/lib.sh

umask 022
export DEBIAN_FRONTEND=noninteractive

ensure_link \
    .aliases \
    .bash_logout \
    .bashrc \
    .profile \
    .zshrc \
    .zprofile \
    .zsh.d

sudo apt-get install -y \
    language-pack-ja \
    zsh

sudo chsh -s "$(which zsh)" "$(whoami)"
