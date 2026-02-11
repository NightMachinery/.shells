#!/usr/bin/env dash
##
PATH="$HOME/scripts/zshlang/wrappers/brishz/:$PATH"
##
tmp="$(mktemp)"
cat > "$tmp" || return $?

brishzq.zsh h-stt-filter "$tmp"
