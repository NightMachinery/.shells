#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: POSIX sh helpers for bootstrap-sudoless.
#: Tier 0: may assume ONLY sh + coreutils + (git|curl). Nothing from zshlang.
#: Sourced by bootstrap.sh and by every stage.

if [ -n "${NIGHT_BOOTSTRAP_LIB_LOADED:-}" ] ; then
    return 0 2>/dev/null || exit 0
fi
NIGHT_BOOTSTRAP_LIB_LOADED=y

##
if [ -t 1 ] && [ -z "${NO_COLOR:-}" ] ; then
    _c_red=$(printf '\033[31m') ; _c_grn=$(printf '\033[32m')
    _c_ylw=$(printf '\033[33m') ; _c_blu=$(printf '\033[34m')
    _c_dim=$(printf '\033[2m')  ; _c_rst=$(printf '\033[0m')
else
    _c_red='' ; _c_grn='' ; _c_ylw='' ; _c_blu='' ; _c_dim='' ; _c_rst=''
fi

log()  { printf '%s==>%s %s\n' "$_c_blu" "$_c_rst" "$*" >&2 ; }
ok()   { printf '%s  ok%s %s\n' "$_c_grn" "$_c_rst" "$*" >&2 ; }
warn() { printf '%swarn%s %s\n' "$_c_ylw" "$_c_rst" "$*" >&2 ; }
err()  { printf '%s err%s %s\n' "$_c_red" "$_c_rst" "$*" >&2 ; }
dim()  { printf '%s     %s%s\n' "$_c_dim" "$*" "$_c_rst" >&2 ; }
die()  { err "$*" ; exit 1 ; }

##
have() { command -v "$1" >/dev/null 2>&1 ; }

#: Run a command, echoing it first. Mirrors [agfi:reval-ec] for tier 0.
run() {
    printf '%s  $ %s%s\n' "$_c_dim" "$*" "$_c_rst" >&2
    "$@"
}

#: Run, but never fail the stage.
#: @warn Must return 0 even on failure. Stages run under `set -e`, so a bare
#: `run_soft foo` that returned non-zero would abort the whole stage -- which
#: is exactly what this helper exists to prevent.
run_soft() {
    if ! run "$@" ; then
        warn "failed (continuing): $*"
    fi
    return 0
}

##
ensure_dir() {
    [ -d "$1" ] || run mkdir -p "$1" || die "cannot create dir: $1"
}

#: fetch URL DEST
fetch() {
    _url="$1" ; _dest="$2"
    ensure_dir "$(dirname "$_dest")"
    if have curl ; then
        curl -fsSL --retry 3 --retry-delay 2 -o "$_dest" "$_url"
    elif have wget ; then
        wget -q -O "$_dest" "$_url"
    else
        die "neither curl nor wget available"
    fi
}

#: fetch_stdout URL
fetch_stdout() {
    if have curl ; then
        curl -fsSL --retry 3 --retry-delay 2 "$1"
    elif have wget ; then
        wget -q -O- "$1"
    else
        die "neither curl nor wget available"
    fi
}

##
#: backup_file PATH
#: Moves PATH aside to PATH.pre-nightsh.<n> so vcsh clone will not refuse.
#: Never overwrites an existing backup.
backup_file() {
    _f="$1"
    [ -e "$_f" ] || [ -L "$_f" ] || return 0

    _n=0
    while [ -e "${_f}.pre-nightsh.${_n}" ] ; do
        _n=$((_n + 1))
        [ "$_n" -gt 50 ] && die "too many backups of $_f"
    done
    run mv -- "$_f" "${_f}.pre-nightsh.${_n}" \
        || die "could not back up $_f"
    dim "backed up $_f -> ${_f}.pre-nightsh.${_n}"
}

##
#: path_prepend DIR  (idempotent, affects only this process tree)
path_prepend() {
    case ":${PATH}:" in
        *":$1:"*) : ;;
        *) PATH="$1:${PATH}" ; export PATH ;;
    esac
}

##
#: is_stage_done NAME / mark_stage_done NAME
#: Stage stamps live in the local cache so a rerun on the same host is cheap,
#: but a new host re-verifies everything (cache is per-host by design).
stage_stamp_dir() { printf '%s/bootstrap-stamps' "${NIGHT_LOCAL_CACHE:-${TMPDIR:-/tmp}}" ; }

is_stage_done() {
    [ -z "${NIGHT_BOOTSTRAP_FORCE:-}" ] || return 1
    [ -e "$(stage_stamp_dir)/$1" ]
}

mark_stage_done() {
    ensure_dir "$(stage_stamp_dir)"
    : > "$(stage_stamp_dir)/$1"
}

##
#: Feature flags. Default off for anything host-specific or heavy.
flag_on() {
    _v=$(eval "printf '%s' \"\${$1:-}\"")
    case "$_v" in
        y|yes|1|true|Y) return 0 ;;
        *) return 1 ;;
    esac
}
