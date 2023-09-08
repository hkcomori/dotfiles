#!/usr/bin/env -S bash -eu -o pipefail

# Run on WSL only
which wslpath &>/dev/null || exit 0

pytest windows/Documents/Keyhac/tests

WIN_APPDATA_DIR="$(powershell.exe -Command '[Environment]::GetFolderPath("ApplicationData")' | sed -e 's/\r//g')"
WSL_APPDATA_DIR="$(wslpath "${WIN_APPDATA_DIR}")"

WIN_DOCUMENTS_DIR="$(powershell.exe -Command '[Environment]::GetFolderPath("MyDocuments")' | sed -e 's/\r//g')"
WSL_DOCUMENTS_DIR="$(wslpath "${WIN_DOCUMENTS_DIR}")"

echo [Install Keyhac config]

rsync -av "$(pwd)/windows/Documents/Keyhac/" "${WSL_APPDATA_DIR}/Keyhac/" \
  --exclude "*/" --include={"*.py","*.pyd"} --exclude "*" --delete
rsync -av "$(pwd)/windows/Documents/Keyhac/extension/" "${WSL_DOCUMENTS_DIR}/Keyhac/extension/" \
  --exclude "tests/" --include={"*.py","*.pyd","*/"} --exclude "*" --delete

# powershell.exe -ExecutionPolicy Bypass -file "$(dirname "$0")"/startup_keyhac.ps1
