#!/usr/bin/env -S bash -eu -o pipefail

# Run on WSL only
which wslpath &>/dev/null || exit 0

TARGET_PATH="${HOME}"/OneDrive
    # shellcheck disable=SC2016
ONEDRIVE_DIR="$(wslpath "$(powershell.exe -Command '$Env:OneDrive' | sed -e 's/\r//g')")"

[ -e "${TARGET_PATH}" ] || ln -sTv "${ONEDRIVE_DIR}" "${TARGET_PATH}"
