#!/usr/bin/env -S bash -eu -o pipefail

# Run on WSL only
which wslpath &>/dev/null || exit 0

WIN_TARGET_DIR="$(powershell.exe -Command '[Environment]::GetFolderPath("MyDocuments")' | sed -e 's/\r//g')"
WSL_TARGET_DIR="$(wslpath "${WIN_TARGET_DIR}")"

echo [Install Keyhac config]

rsync -av "$(pwd)/windows/AppData/Keyhac/" "${WSL_TARGET_DIR}/Keyhac/" \
  --exclude "tests/*" --include={"*.py","*/"} --exclude "*"

# powershell.exe -ExecutionPolicy Bypass -file "$(dirname "$0")"/startup_keyhac.ps1
