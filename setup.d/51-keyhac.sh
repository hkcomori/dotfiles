#!/usr/bin/env -S bash -eu -o pipefail

# Run on WSL only
which wslpath &>/dev/null || exit 0

WSL_APPDATA_DIR="$(wslpath "$(
  powershell.exe -Command '[Environment]::GetFolderPath("ApplicationData")' \
  | nkf \
  | sed -e 's/\r//g'
)")"

WSL_DOCUMENTS_DIR="$(wslpath "$(
  powershell.exe -Command '[Environment]::GetFolderPath("MyDocuments")' \
  | nkf \
  | sed -e 's/\r//g'
)")"

echo [Install Keyhac config]

rsync -av "$(pwd)/windows/Documents/Keyhac/" "${WSL_APPDATA_DIR}/Keyhac/" \
  --exclude "*/" --include={"*.py","*.pyd"} --exclude "*" --delete
rsync -av "$(pwd)/windows/Documents/Keyhac/extension/" "${WSL_DOCUMENTS_DIR}/Keyhac/extension/" \
  --exclude "tests/" --include={"*.py","*.pyd","*/"} --exclude "*" --delete

# powershell.exe -ExecutionPolicy Bypass -file "$(dirname "$0")"/startup_keyhac.ps1
