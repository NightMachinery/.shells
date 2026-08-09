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
night_fqdn="$(hostname -f 2>/dev/null || printf '%s' "$night_host")"

night_profile_detect() {
    case "${night_host}" in
        beta|rho[0-9]*|zeta[0-9]*|epsilon[0-9]*|pi)
            #: Confirm it really is the CIS cluster and not a namesake host.
            if [ -d /mounts/Users ] ; then
                printf 'cis-lmu' ; return 0
            fi
            ;;
    esac
    case "${night_fqdn}" in
        *.cis.lmu.de|*.cis.uni-muenchen.de) printf 'cis-lmu' ; return 0 ;;
    esac
    printf 'default'
}

: "${NIGHT_PROFILE:="$(night_profile_detect)"}"

night_profile_file="${NIGHT_BOOTSTRAP_DIR}/profiles/${NIGHT_PROFILE}.sh"
if [ ! -r "${night_profile_file}" ] ; then
    printf 'profile not found: %s\n' "${night_profile_file}" >&2
    exit 1
fi
# shellcheck disable=SC1090
. "${night_profile_file}"

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
