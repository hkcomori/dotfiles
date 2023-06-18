#!/usr/bin/env -S bash -eu -o pipefail

# Run on WSL only
which wslpath &>/dev/null || exit 0

WIN_DOCUMENT_DIR="$(powershell.exe -Command '[Environment]::GetFolderPath("MyDocuments")' | sed -e 's/\r//g')"
WSL_DOCUMENT_DIR="$(wslpath "${WIN_DOCUMENT_DIR}")"

echo [Install AHK Scripts]

rsync -av "$(pwd)/windows/Documents/AutoHotKey/" "${WSL_DOCUMENT_DIR}/AutoHotKey/" \
  --exclude "*.spec.ahk" --include={"*.ahk","*/"} --exclude "*"

powershell.exe -ExecutionPolicy Bypass -file "$(dirname "$0")"/startup_autohotkey.ps1
