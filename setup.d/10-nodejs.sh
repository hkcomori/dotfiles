#!/usr/bin/env -S bash -eu -o pipefail

umask 022
export DEBIAN_FRONTEND=noninteractive

node -v && exit 0

sudo apt-get install -y \
    nodejs \
    npm

npm config set prefix "$HOME/.local"

npm install -g n
N_PREFIX="$HOME/.local" n stable

sudo apt-get purge -y \
    nodejs \
    npm
