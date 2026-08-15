#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Stage 05: system packages, for hosts where we have root.
#:
#: This is the *only* stage that needs privilege, and it is a no-op without it
#: -- so the sudo-less flavour runs the identical stage list and simply skips
#: here. That is the whole point of splitting on a capability rather than
#: forking the tree: there is one set of stages, not two.
#:
#: Runs at 05, i.e. after 00-dirs (which defines the storage contract) and
#: before 10-vcsh-dotfiles, because everything downstream assumes git and curl.
#:
#: What belongs here: only things that (a) a stage hard-requires and (b) cannot
#: be had in user space. Anything mise or conda can provide belongs in stage 20
#: or 40 instead, where it works on every host rather than only the rooted ones.

# shellcheck disable=SC1091
. "${HOME}/.night-bootstrap.env"

if ! flag_on NIGHT_SUDO ; then
    ok "no root on this host; user-space stages will cover what they can"
    return 0 2>/dev/null || exit 0
fi

_pm="$(pkg_manager)"
[ -n "${_pm}" ] || { warn "root, but no known package manager; skipping" ; return 0 2>/dev/null || exit 0 ; }
log "package manager: ${_pm}"

##
#: --- tier 0's own dependencies ---
#: bootstrap.sh already got this far, so these are usually present; installing
#: them is for the container-minimal case where one is missing.
pkg_install git curl ca-certificates

##
#: --- zsh ---
#: Stage 30 hard-dies without it, and there is no good user-space fallback:
#: building zsh from source needs a toolchain we may not have yet, and the
#: conda build lags. On CIS this is preinstalled, which is why the sudo-less
#: flavour never needed it.
pkg_ensure zsh zsh || warn "zsh still missing; stage 30 will fail"

##
#: --- a C toolchain ---
#: Emacs native compilation needs one, and so does anything mise builds rather
#: than downloads. Stage 50 otherwise pulls sysroot_linux-64/gcc_linux-64 from
#: conda-forge, which works but is slower and duplicates what the distro has.
case "${_pm}" in
    apt-get)      pkg_install build-essential ;;
    dnf|yum)      pkg_install gcc gcc-c++ make ;;
    pacman)       pkg_install base-devel ;;
    zypper)       pkg_install gcc gcc-c++ make ;;
    apk)          pkg_install build-base ;;
esac

##
#: --- small conveniences the stages shell out to ---
#: All optional: every one of these has a mise or conda path in a later stage,
#: so a failure here costs nothing.
pkg_install rsync tmux unzip

##
ok "system packages done"
