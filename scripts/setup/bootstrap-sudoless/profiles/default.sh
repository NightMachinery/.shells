#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Profile: generic sudo-less Linux host with no special storage layout.
#: Everything lives under $HOME; no assumptions about shared filesystems.

NIGHT_PROFILE_NAME='default'

: "${NIGHT_BIG_STORE:=${HOME}/big}"
: "${NIGHT_LOCAL_CACHE:=${TMPDIR:-/tmp}/${USER}}"
: "${NIGHT_BIN:=${HOME}/.local/bin}"
: "${BOOTSTRAP_WITH_PROXY:=n}"
