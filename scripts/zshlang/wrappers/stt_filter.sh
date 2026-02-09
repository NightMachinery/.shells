#!/usr/bin/env dash

tmp="$(mktemp)"
cat > "$tmp" || return $?

brishzq.zsh h-stt-filter "$tmp"
