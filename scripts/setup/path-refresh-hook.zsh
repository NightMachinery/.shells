#!/bin/zsh -f
# The installer links this file into the hooks directory of vcsh night.sh.
unsetopt BG_NICE
root="${${0:A}:h:h}"
source "${root}/zshlang/basic/auto-load/path-cache.zsh" || exit 1
h-night-path-cache-file
h-night-path-refresh "${root}" "${REPLY}" hook </dev/null >/dev/null 2>&1 &!
