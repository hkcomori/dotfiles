#!/usr/bin/env -S bash -eu -o pipefail

# Ensure symlinks into the home directory
#
# Usage:
#   ensure_link <src>...
#
# Args:
#   - src: Related path of files or directories
ensure_link() {
    for file; do
        ensure_intermediate_dir "${file}" "${HOME}"
        target="${HOME}"/"${file}"
        [ -e "${target}" ] || ln -sTv "$(pwd)"/"${file}" "${target}"
        chmod -v "$(stat -c '%a' "${file}")" "${target}"
    done
}

# Ensure copy files into the home directory
#
# Usage:
#   ensure_copy <src>...
#
# Args:
#   - src: Related path of files or directories
ensure_copy() {
    for file; do
        ensure_intermediate_dir "${file}" "${HOME}"
        target="${HOME}"/"${file}"
        cp --preserve=mode --update=none -v "$(pwd)"/"${file}" "${target}"
    done
}

# Ensure intermediate directories and set permissions to the same as references
#
# Usage:
#   ensure_intermediate_dir <src> <dest>
#
# Args:
#   - src: Related path of source file
#   - dest: Destination directory
ensure_intermediate_dir() {
    reference="$(dirname "$1")"
    root="$(readlink -f "$2")"
    target="${root}"/"${reference}"
    mkdir -pv "${target}"
    while [ "${root}" != "$(readlink -f "${target}")" ]; do
        chmod -v "$(stat -c '%a' "${reference}")" "${target}"
        reference="$(dirname "${reference}")"
        target="${root}"/"${reference}"
    done
}
