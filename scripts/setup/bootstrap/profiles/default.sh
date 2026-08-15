#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Profile: a plain sudo-less Linux host. This is the default, and it should
#: stay usable on a machine nobody has ever configured: everything lives under
#: $HOME, and nothing is assumed about shared filesystems or other users.
#:
#: Set the two capability flags below if they apply to your host; they are what
#: the stages branch on, not the profile name:
#:
#:   NIGHT_HOME_SHARED=y  $HOME is one filesystem across several machines.
#:     Then: per-host state (emacs sockets, caches, redis data) must go to
#:     local disk, and anything written to $HOME must derive host-specific
#:     values rather than hardcode them.
#:   NIGHT_MULTIUSER=y  other people can log in here.
#:     Then: umask 077, mode 700 directories, and services that listen on
#:     loopback need authentication -- 127.0.0.1 excludes other hosts, not
#:     other users of this one.

NIGHT_PROFILE_NAME='default'

#: `:=' so a one-off run can override without inventing a whole profile:
#:   NIGHT_MULTIUSER=n sh bootstrap.sh
: "${NIGHT_HOME_SHARED:=n}"

#: Defaults to y on purpose: this is the fail-safe direction. Guessing
#: "multiuser" on a private box costs a stricter umask and mode 700 on our
#: own directories -- no lost capability. Guessing "private" on a box that
#: turns out to be shared leaves our history, tokens and redis data readable
#: by everyone on it, and we would never notice. Set n only when you know.
: "${NIGHT_MULTIUSER:=y}"

: "${NIGHT_BIG_STORE:=${HOME}/big}"
: "${NIGHT_LOCAL_CACHE:=${TMPDIR:-/tmp}/${USER}}"
: "${NIGHT_BIN:=${HOME}/.local/bin}"
: "${BOOTSTRAP_WITH_PROXY:=n}"
