#!/usr/bin/env -S bash -e -o pipefail

# Run on WSL only
which wslpath &>/dev/null || exit 0

echo [Install AHK Scripts]

rsync -av "$(pwd)/win_home/Documents/AutoHotKey/" "${WIN_HOME}/Documents/AutoHotKey/" \
  --exclude "*.spec.ahk" --include={"*.ahk","*/"} --exclude "*"

powershell.exe -ExecutionPolicy Bypass -file "$(dirname "$0")"/startup_autohotkey.ps1
