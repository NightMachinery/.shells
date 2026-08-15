#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Bootstrap this host, declaring that we do NOT have root.
#:
#: This is the original behaviour and the conservative one: stage 05 becomes a
#: no-op and every dependency is satisfied in user space (mise, conda, static
#: binaries under ~/.local/bin). It is what the LMU CIS cluster needs, and what
#: any shared login node needs.
#:
#: Worth choosing *explicitly* even on a host that happens to have sudo: it
#: keeps the machine's system state untouched, which is the polite default on
#: anything you do not own.
#:
#: Usage: identical to bootstrap.sh.
#:   sh bootstrap-sudoless.sh
#:   sh bootstrap-sudoless.sh 50
#:   BOOTSTRAP_CIS_P=y sh bootstrap-sudoless.sh

set -eu

_dir="$(cd "$(dirname "$0")" && pwd)"

NIGHT_SUDO=n
export NIGHT_SUDO

exec sh "${_dir}/bootstrap.sh" "$@"
