#!/usr/bin/env -S bash -e -o pipefail

AHK_LIB_DIR="${WIN_HOME}/Documents/AutoHotKey/Lib"

echo [Install Yunit]

mkdir -p "${AHK_LIB_DIR}"
cd "${AHK_LIB_DIR}"
git clone https://github.com/Uberi/Yunit 2>/dev/null \
  || (cd Yunit; git pull)
