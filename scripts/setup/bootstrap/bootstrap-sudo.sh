#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Bootstrap this host, declaring that we DO have root.
#:
#: Identical to bootstrap-sudoless.sh in every way except that stage 05 is
#: allowed to install system packages. Same driver, same stages, same
#: profiles -- see README.org, "One tree, two flavours".
#:
#: Use this when you know the host has passwordless sudo (a cloud VM, a
#: container, your own box) and you want the distro to supply zsh and a C
#: toolchain instead of conda.
#:
#: Usage: identical to bootstrap.sh.
#:   sh bootstrap-sudo.sh
#:   sh bootstrap-sudo.sh 50
#:   NIGHT_BOOTSTRAP_FORCE=y sh bootstrap-sudo.sh 05

set -eu

_dir="$(cd "$(dirname "$0")" && pwd)"

#: Declared, not probed: the whole point of choosing this entry point is to say
#: so. If the claim is wrong, `sudo -n` fails fast and pkg_install degrades to
#: a warning rather than hanging on a password prompt.
NIGHT_SUDO=y
export NIGHT_SUDO

exec sh "${_dir}/bootstrap.sh" "$@"
