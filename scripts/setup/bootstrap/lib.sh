#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: POSIX sh helpers for the bootstrap driver.
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
#: --- root, and system packages ---
#: Everything below is a no-op unless NIGHT_SUDO=y (see profile.sh). The
#: sudo-less flavour must behave exactly as it did before this existed, so no
#: caller needs to branch: `pkg_install foo` simply reports and returns 0.

#: Never a bare `sudo`. -n so a missing/expired credential fails immediately
#: instead of blocking on a password prompt nobody is watching.
as_root() {
    if [ "$(id -u)" -eq 0 ] ; then
        run "$@"
    else
        run sudo -n "$@"
    fi
}

#: as_root, but never fail the stage -- the root-flavoured `run_soft`.
#: @warn Do NOT write `run_soft as_root ...`: both helpers echo, so the command
#: is printed twice (once as `as_root env ...`, once as `sudo -n env ...`).
#: as_root already echoes; this only adds the swallow-and-warn.
as_root_soft() {
    if ! as_root "$@" ; then
        warn "failed (continuing): $*"
    fi
    return 0
}

#: Which system package manager, if any. Printed name doubles as the guard:
#: empty means "no idea", and pkg_install then declines rather than guessing.
pkg_manager() {
    if [ -n "${NIGHT_PKG_MANAGER:-}" ] ; then printf '%s' "${NIGHT_PKG_MANAGER}" ; return 0 ; fi
    for _pm in apt-get dnf yum pacman zypper apk ; do
        if have "$_pm" ; then printf '%s' "$_pm" ; return 0 ; fi
    done
    printf ''
}

#: pkg_refresh -- update the package index at most once per bootstrap run.
pkg_refresh() {
    flag_on NIGHT_SUDO || return 0
    [ -z "${NIGHT_PKG_REFRESHED:-}" ] || return 0
    NIGHT_PKG_REFRESHED=y

    case "$(pkg_manager)" in
        apt-get) as_root_soft env DEBIAN_FRONTEND=noninteractive apt-get update -qq ;;
        dnf|yum) : ;;   #: refresh implicitly on install
        pacman)  as_root_soft pacman -Sy --noconfirm ;;
        zypper)  as_root_soft zypper --non-interactive refresh ;;
        apk)     as_root_soft apk update ;;
        *)       : ;;
    esac
    return 0
}

#: pkg_install PKG...  -- best effort, never fatal.
#: @warn Must return 0 even when it installs nothing. Stages run under `set -e`,
#: and a missing system package is a reason to fall back to the user-space
#: path (mise, conda), not a reason to abort the bootstrap.
pkg_install() {
    [ "$#" -gt 0 ] || return 0

    if ! flag_on NIGHT_SUDO ; then
        dim "no sudo; skipping system packages: $*"
        return 0
    fi

    _pm="$(pkg_manager)"
    if [ -z "$_pm" ] ; then
        warn "no known package manager; skipping system packages: $*"
        return 0
    fi

    pkg_refresh
    case "$_pm" in
        apt-get) as_root_soft env DEBIAN_FRONTEND=noninteractive apt-get install -y -qq "$@" ;;
        dnf)     as_root_soft dnf install -y "$@" ;;
        yum)     as_root_soft yum install -y "$@" ;;
        pacman)  as_root_soft pacman -S --noconfirm --needed "$@" ;;
        zypper)  as_root_soft zypper --non-interactive install "$@" ;;
        apk)     as_root_soft apk add "$@" ;;
    esac
    return 0
}

#: pkg_ensure CMD PKG...  -- install PKG... only if CMD is still missing.
#: The common shape: we want the *command*, and the package is just how to
#: get it. Keeps stages from re-installing on every run.
pkg_ensure() {
    _cmd="$1" ; shift
    if have "$_cmd" ; then
        ok "${_cmd} already present: $(command -v "$_cmd")"
        return 0
    fi
    pkg_install "$@"
    have "$_cmd" || return 1
    ok "${_cmd} installed: $(command -v "$_cmd")"
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
