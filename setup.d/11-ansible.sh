#!/usr/bin/env -S bash -eu -o pipefail

umask 022
export DEBIAN_FRONTEND=noninteractive

ansible --version && exit 0
sudo apt-get install -y ansible ansible-lint
