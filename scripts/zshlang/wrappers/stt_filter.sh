#!/usr/bin/env dash
##
brishzgo="${BRISHZGO_BIN:-${HOME}/go/bin/brishzgo}"
##
tmp="$(command mktemp)" || exit $?
trap 'command rm -f -- "$tmp"' EXIT HUP INT TERM
command cat > "$tmp" || exit $?

brishz_async= command "${brishzgo}" -- h-stt-filter "$tmp"
