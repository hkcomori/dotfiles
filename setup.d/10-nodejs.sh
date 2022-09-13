#!/usr/bin/env -S bash -e -o pipefail

umask 022
export DEBIAN_FRONTEND=noninteractive

node -v && exit 0

sudo apt-get install -y \
    nodejs \
    npm

sudo npm install n --location=global
sudo n stable

sudo apt-get purge -y \
    nodejs \
    npm
