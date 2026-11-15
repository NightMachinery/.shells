#!/bin/sh
set -eu

if [ "$#" -ne 2 ]; then
    echo "usage: build.sh <lua-include-dir> <output-so>" >&2
    exit 2
fi

include_dir=$1
output=$2
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
out_dir=$(dirname -- "$output")
tmp_out="${output}.tmp.$$"

mkdir -p "$out_dir"
trap 'rm -f "$tmp_out"' EXIT INT TERM

/usr/bin/xcrun clang \
    -O2 \
    -fobjc-arc \
    -Wall -Wextra -Werror \
    -bundle \
    -I "$include_dir" \
    -undefined dynamic_lookup \
    -framework Foundation \
    -framework CoreGraphics \
    -framework CoreText \
    "$script_dir/module.m" \
    -o "$tmp_out"

mv -f "$tmp_out" "$output"
