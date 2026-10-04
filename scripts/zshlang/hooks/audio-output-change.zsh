#!/usr/bin/env zsh -f

command "${BRISHZGO_BIN:-${HOME}/go/bin/brishzgo}" -- h-hook-audio-output-change "$@"
