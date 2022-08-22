#!/usr/bin/env -S bash -e -o pipefail

echo [Install AHK Scripts]

rsync -av "$(pwd)/win_home/Documents/AutoHotKey/" "${WIN_HOME}/Documents/AutoHotKey/" \
  --exclude "*.spec.ahk" --include={"*.ahk","*/"} --exclude "*"
