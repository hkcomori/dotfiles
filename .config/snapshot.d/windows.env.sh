# shellcheck shell=sh
# This script is designed for use within the Windows Subsystem for Linux (WSL).
# Set additional environment variables for systemd snapshot@.service.

# Define a user-specific systemd environment variable.
set_env() {
    systemctl --user set-environment "$1"
}

# Convert a Windows path argument to a clean UTF-8 Unix path.
wslpath_u() {
    wslpath -u "$1" \
    | sed -e 's/\r//g' \
    | iconv -f sjis -t utf8
}

# Resolve the relative path of an AppX package's local data directory.
relative_package_dir() {
    wslpath_u "$(\
        powershell.exe -Command \
            '${env:LOCALAPPDATA} + "\Packages\" + '\(Get-AppxPackage -Name "$1"\).PackageFamilyName
    )" \
    | sed -e 's/^\/mnt\///g'
}


# Windows Terminal settings file location
set_env WINDOWS_TERMINAL_SETTINGS="$(relative_package_dir "Microsoft.WindowsTerminal")/LocalState/settings.json"
