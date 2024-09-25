#!/usr/bin/env -S bash -eu -o pipefail

umask 022
export DEBIAN_FRONTEND=noninteractive

node -v && exit 0

sudo apt-get install -y \
    nodejs \
    npm

# Install packages in your home directory with `npm install -g`
ln -sr ~/.local/lib/node_modules ~/.node_modules

npm install -g n
n stable

sudo apt-get purge -y \
    nodejs \
    npm

npm config set prefix "$HOME/.local"
