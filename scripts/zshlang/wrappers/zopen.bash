#!/usr/local/bin/bash

echo starting with "$@"

#: The garden owns the command environment; the Go worker detaches locally.
brishz_async=y exec "${BRISHZGO_BIN:-${HOME}/go/bin/brishzgo}" -- awaysh-named JOKER_MARKER zopen "$@"
