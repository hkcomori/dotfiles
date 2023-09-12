#!/usr/bin/env -S bash -eu -o pipefail

umask 022
export DEBIAN_FRONTEND=noninteractive

sudo apt-get install -y \
    doxygen \
    graphviz
