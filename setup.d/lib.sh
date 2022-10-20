#!/usr/bin/env -S bash -eu -o pipefail

ensure_link() {
    for file; do
        target="${HOME}"/"${file}"
        parent="$(dirname "${target}")"
        if [ "${HOME}" != "$(readlink -f "${parent}")" ]; then
            mkdir -pv "${parent}"
            chmod -v "$(stat -c '%a' "${parent}")" "${parent}"
        fi
        [ -e "${target}" ] || ln -sTv "$(pwd)"/"${file}" "${target}"
        chmod -v "$(stat -c '%a' "${file}")" "${target}"
    done
}

ensure_copy() {
    for file; do
        target="${HOME}"/"${file}"
        parent="$(dirname "${target}")"
        if [ "${HOME}" != "$(readlink -f "${parent}")" ]; then
            mkdir -pv "${parent}"
            chmod -v "$(stat -c '%a' "${parent}")" "${parent}"
        fi
        cp --preserve=mode -nv "$(pwd)"/"${file}" "${target}"
    done
}
