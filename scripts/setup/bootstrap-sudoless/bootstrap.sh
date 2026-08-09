#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Sudo-less bootstrap entry point.
#:
#: Supersedes setup/minimal_proxy/, which entangled "minimal vs full" with
#: "proxy vs no proxy". Here the proxy is just one optional stage.
#:
#: Usage:
#:   sh bootstrap.sh                 # run all default stages
#:   sh bootstrap.sh 10 20           # run only stages whose name starts with these
#:   sh bootstrap.sh --list
#:   NIGHT_BOOTSTRAP_FORCE=y sh bootstrap.sh 50   # ignore completion stamps
#:
#: Storage layout is chosen by profile.sh from the hostname; override with
#:   NIGHT_BIG_STORE=/nfs/gdata/$USER sh bootstrap.sh

set -eu

NIGHT_BOOTSTRAP_DIR="$(cd "$(dirname "$0")" && pwd)"
export NIGHT_BOOTSTRAP_DIR

# shellcheck disable=SC1091
. "${NIGHT_BOOTSTRAP_DIR}/lib.sh"
# shellcheck disable=SC1091
. "${NIGHT_BOOTSTRAP_DIR}/profile.sh"

##
usage() {
    sed -n '3,20p' "$0" | sed 's/^#: \{0,1\}//' >&2
    exit 0
}

list_stages() {
    for _s in "${NIGHT_BOOTSTRAP_DIR}"/stages/[0-9]*.sh ; do
        [ -e "$_s" ] || continue
        printf '  %s\n' "$(basename "$_s" .sh)"
    done
    printf '\noptional (opt in with the matching flag):\n'
    for _s in "${NIGHT_BOOTSTRAP_DIR}"/stages/optional/*.sh ; do
        [ -e "$_s" ] || continue
        printf '  %s\n' "$(basename "$_s" .sh)"
    done
}

##
case "${1:-}" in
    -h|--help) usage ;;
    --list) list_stages ; exit 0 ;;
esac

##
log "profile: ${NIGHT_PROFILE_NAME}  host: ${night_host}"
dim "NIGHT_BIN         = ${NIGHT_BIN}"
dim "NIGHT_BIG_STORE   = ${NIGHT_BIG_STORE}"
dim "NIGHT_LOCAL_CACHE = ${NIGHT_LOCAL_CACHE}"
dim "NIGHTDIR          = ${NIGHTDIR}"

#: ~/.local/bin must be usable before anything installs into it.
path_prepend "${NIGHT_BIN}"

##
stage_matches() {
    #: With no args, every stage matches.
    [ "$#" -gt 1 ] || return 0
    _name="$1" ; shift
    for _pat in "$@" ; do
        case "${_name}" in
            "${_pat}"*) return 0 ;;
        esac
    done
    return 1
}

failed_stages=''

for stage in "${NIGHT_BOOTSTRAP_DIR}"/stages/[0-9]*.sh ; do
    [ -e "${stage}" ] || continue
    stage_name="$(basename "${stage}" .sh)"

    stage_matches "${stage_name}" "$@" || continue

    if is_stage_done "${stage_name}" ; then
        ok "${stage_name} (already done; NIGHT_BOOTSTRAP_FORCE=y to redo)"
        continue
    fi

    log "stage ${stage_name}"
    #: Stages run in a subshell so one stage cannot corrupt the driver's state,
    #: and so `set -e` inside a stage does not kill the whole run.
    if ( set -eu ; . "${stage}" ) ; then
        mark_stage_done "${stage_name}"
        ok "${stage_name}"
    else
        err "${stage_name} FAILED"
        failed_stages="${failed_stages} ${stage_name}"
    fi
done

##
if [ -n "${failed_stages}" ] ; then
    err "failed stages:${failed_stages}"
    err "rerun a single stage with: sh bootstrap.sh <prefix>"
    exit 1
fi

log "done. Run doctor.sh to verify."
