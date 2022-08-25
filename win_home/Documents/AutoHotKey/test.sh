#!/usr/bin/env -S bash -e -o pipefail

cd "$(dirname $0)"

find . -name "*.spec.ahk" -print0 | xargs -0 -I{} AutoHotkeyU64.exe {}
