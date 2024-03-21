#!/usr/bin/env -S bash -eu -o pipefail

. "$(dirname "$0")"/lib.sh

umask 022
export DEBIAN_FRONTEND=noninteractive

ensure_link \
    .config/flake8 \
    .config/pep8

sudo apt-get install -y python3-dev python3-venv python3.11-dev python3.11-venv
sudo update-alternatives --install /usr/bin/python python "$(which python3.11)" 1
