#!/usr/bin/env -S bash -eu -o pipefail

. "$(dirname "$0")"/lib.sh

umask 022
export DEBIAN_FRONTEND=noninteractive

ensure_link \
    .config/flake8 \
    .config/pep8

autopep8 --version && flake8 --version && mypy --version && exit 0
sudo apt-get install -y python3 python3-pip
sudo -HE pip3 install autopep8 flake8 mypy poetry pytest
