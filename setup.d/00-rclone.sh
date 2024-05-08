#!/usr/bin/env -S bash -eu -o pipefail
#
# Install RClone
#
# https://rclone.org/install/#script-installation

sudo -v
curl https://rclone.org/install.sh | sudo bash
