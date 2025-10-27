#!/usr/bin/env -S bash -eu -o pipefail
# https://docs.docker.com/engine/install/ubuntu/#installation-methods

umask 022
export DEBIAN_FRONTEND=noninteractive

docker --version && exit 0

# Set up Docker's apt repository.
sudo apt-get install -y --no-install-recommends ca-certificates curl gnupg
sudo install -m 0755 -d /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
sudo chmod a+r /etc/apt/keyrings/docker.gpg

echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update

# Install the Docker packages.
sudo apt-get install -y --no-install-recommends docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

# Add the user to the docker group
sudo usermod -aG docker $USER
