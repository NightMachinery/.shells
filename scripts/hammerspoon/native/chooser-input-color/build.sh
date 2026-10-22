#!/bin/sh
# Build into the caller's private cache, never into the source tree.
set -eu
bundle=$1
output=$2
source_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
command mkdir -p "$(dirname -- "$output")"
staging=$(command mktemp "${output}.XXXXXX")
trap 'command rm -f "$staging"' EXIT HUP INT TERM
/usr/bin/xcrun clang -O2 -fobjc-arc -Wall -Wextra -Werror -bundle \
    -F "$bundle/Contents/Frameworks" -framework Cocoa -framework LuaSkin \
    -Wl,-rpath,"$bundle/Contents/Frameworks" \
    "$source_dir/module.m" -o "$staging"
command mv -f "$staging" "$output"
