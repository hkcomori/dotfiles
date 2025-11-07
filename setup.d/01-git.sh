#!/usr/bin/env -S bash -eu -o pipefail

. "$(dirname "$0")"/lib.sh

umask 022
export DEBIAN_FRONTEND=noninteractive

ensure_link \
    .config/git/hkcomori.conf \
    .config/git/windows.conf \
    .config/git/config

ensure_copy \
    work/.gitkeep \
    mnt/.gitkeep

sudo apt-get install -y \
    git

if command -v "git-credential-manager" > /dev/null 2>&1; then
    echo git-credential-manager is already installed
else
    dotnet tool install -g \
        git-credential-manager
fi
