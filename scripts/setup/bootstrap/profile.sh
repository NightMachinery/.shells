#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Hostname -> profile dispatch, and the storage contract every stage consumes.
#: No stage may hardcode a storage path; they all read the NIGHT_* vars below.

if [ -n "${NIGHT_BOOTSTRAP_PROFILE_LOADED:-}" ] ; then
    return 0 2>/dev/null || exit 0
fi
NIGHT_BOOTSTRAP_PROFILE_LOADED=y

: "${NIGHT_BOOTSTRAP_DIR:="$(cd "$(dirname "$0")" && pwd)"}"

#: Bare hostname; these hosts report e.g. "beta", not "beta.cis.lmu.de".
night_host="$(hostname -s 2>/dev/null || hostname)"

#: --- which site are we on? ---
#:
#: Declared, never inferred. Every inference was tried and each was wrong:
#: a mount point only proves the share is mounted; a $HOME path prefix is a
#: naming convention, not an identity; the DNS search domain tracks network
#: connectivity (the laptop matched it over VPN); hostnames need per-machine
#: upkeep and collide -- "beta" is not a rare name.
#:
#: Resolution order, first hit wins:
#:   1. NIGHT_PROFILE=<name>     explicit, and what the others reduce to
#:   2. BOOTSTRAP_CIS_P=y        shorthand for the CIS cluster
#:   3. ~/.night-site            written by a previous run; since $HOME is
#:                               shared on such clusters, one file covers
#:                               every host in it
#:   4. default                  a plain sudo-less host: everything under $HOME
#:
#: So a brand new sudo-less machine needs no configuration at all, and a CIS
#: machine needs BOOTSTRAP_CIS_P=y exactly once.
night_site_file="${HOME}/.night-site"

night_profile_detect() {
    if [ -n "${BOOTSTRAP_CIS_P:-}" ] ; then
        printf 'cis-lmu' ; return 0
    fi
    if [ -r "${night_site_file}" ] ; then
        #: first non-comment, non-blank line
        sed -e 's/#.*//' -e '/^[[:space:]]*$/d' "${night_site_file}" 2>/dev/null \
            | head -1 | tr -d '[:space:]'
        return 0
    fi
    printf 'default'
}

: "${NIGHT_PROFILE:="$(night_profile_detect)"}"
: "${NIGHT_PROFILE:=default}"

night_profile_file="${NIGHT_BOOTSTRAP_DIR}/profiles/${NIGHT_PROFILE}.sh"
if [ ! -r "${night_profile_file}" ] ; then
    printf 'profile not found: %s\n' "${night_profile_file}" >&2
    exit 1
fi
# shellcheck disable=SC1090
. "${night_profile_file}"

#: --- capabilities, not site names ---
#: Stages must never ask "am I on CIS?". They ask what they actually care
#: about, and a profile answers. Adding a new cluster is then one profile file
#: with no changes anywhere else.
#: Last-resort defaults, for a profile that declares neither. Only n is safe
#: to assume for HOME_SHARED (guessing "shared" would scatter state to local
#: disk for no reason), whereas MULTIUSER must default to y: a profile that
#: forgot to say should get the stricter behaviour, not the laxer one.
: "${NIGHT_HOME_SHARED:=n}"   #: $HOME is one filesystem across many hosts
: "${NIGHT_MULTIUSER:=y}"     #: other people can log into this machine

#: --- may we install system packages? ---
#: The one capability it is safe to *probe* rather than declare, because the
#: probe is the operation itself: `sudo -n true` succeeds only if this very
#: process can already run root commands without a prompt. That is not an
#: inference about the host, it is a direct test of the thing stages need.
#:
#: Contrast the site profile, which is declared precisely because every probe
#: for it was a proxy for the real question. Here there is no proxy.
#:
#: @warn Must never *prompt*. A password prompt on a headless host is a hung
#: bootstrap, not a question -- the same failure mode as GIT_TERMINAL_PROMPT.
#: Hence `-n` (non-interactive) everywhere, never a bare `sudo`.
night_sudo_detect() {
    #: Already root: no sudo needed, and none may exist (containers).
    [ "$(id -u)" -eq 0 ] && { printf 'y' ; return 0 ; }
    command -v sudo >/dev/null 2>&1 || { printf 'n' ; return 0 ; }
    if sudo -n true >/dev/null 2>&1 ; then printf 'y' ; else printf 'n' ; fi
}
: "${NIGHT_SUDO:="$(night_sudo_detect)"}"

export NIGHT_HOME_SHARED NIGHT_MULTIUSER NIGHT_SUDO
export night_site_file

export NIGHT_PROFILE NIGHT_PROFILE_NAME
export NIGHT_BIG_STORE NIGHT_LOCAL_CACHE NIGHT_BIN
export night_host night_fqdn

#: Where the dotfiles repo lands. NIGHTDIR is what zshlang itself uses.
: "${NIGHTDIR:=${HOME}/scripts}"
export NIGHTDIR

#: Upstream of the dotfiles. HTTPS so no key is needed for the initial clone.
: "${NIGHT_DOTFILES_URL:=https://github.com/NightMachinery/.shells.git}"
: "${NIGHT_DOTFILES_VCSH_NAME:=night.sh}"
export NIGHT_DOTFILES_URL NIGHT_DOTFILES_VCSH_NAME
