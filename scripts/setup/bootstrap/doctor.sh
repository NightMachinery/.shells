#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Report what this host has, what it is missing, and whether the shared-home
#: assumptions still hold. Read-only: it never installs or changes anything.
#:
#: Because $HOME is shared across every CIS host, most of the state this
#: reports is identical everywhere; what genuinely differs per host is the
#: local cache and whatever a stage compiled into it.

NIGHT_BOOTSTRAP_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck disable=SC1091
. "${NIGHT_BOOTSTRAP_DIR}/lib.sh"
# shellcheck disable=SC1091
. "${NIGHT_BOOTSTRAP_DIR}/profile.sh"

[ -r "${HOME}/.night-bootstrap.env" ] && . "${HOME}/.night-bootstrap.env"
path_prepend "${NIGHT_BIN}"
[ -n "${MISE_DATA_DIR:-}" ] && path_prepend "${MISE_DATA_DIR}/shims"

problems=0
note_problem() { problems=$((problems + 1)) ; }

##
log "host"
dim "hostname:  ${night_host} (${night_fqdn})"
dim "profile:   ${NIGHT_PROFILE_NAME}"
dim "kernel:    $(uname -r)"
dim "cores:     $(nproc 2>/dev/null || echo '?')"
if have nvidia-smi ; then
    dim "gpu:       $(nvidia-smi --query-gpu=name --format=csv,noheader 2>/dev/null | sort -u | tr '\n' ' ')"
fi

##
log "storage"
for pair in "NIGHT_BIN:${NIGHT_BIN}" "NIGHT_BIG_STORE:${NIGHT_BIG_STORE}" "NIGHT_LOCAL_CACHE:${NIGHT_LOCAL_CACHE}" ; do
    name="${pair%%:*}" ; path="${pair#*:}"
    if [ -d "${path}" ] && [ -w "${path}" ] ; then
        dim "$(printf '%-18s %s  (%s free)' "${name}" "${path}" \
            "$(df -h "${path}" 2>/dev/null | awk 'NR==2{print $4}')")"
    else
        err "${name} missing or not writable: ${path}"
        note_problem
    fi
done

#: The home quota is the one that actually bites.
if have quota ; then
    q="$(quota -s 2>/dev/null | awk 'NR==3{print $2" used of "$3}')"
    [ -n "${q}" ] && dim "home quota:        ${q}"
fi

##
log "tools"
for b in zsh git tmux vcsh mise rg fd fzf jq bat delta eza zoxide starship gh uv micromamba emacs ; do
    if have "${b}" ; then
        dim "$(printf '%-12s %s' "${b}" "$(command -v "${b}")")"
    else
        warn "$(printf '%-12s MISSING' "${b}")"
        note_problem
    fi
done

##
log "dotfiles"
if [ -d "${NIGHTDIR}" ] ; then
    dim "NIGHTDIR:  ${NIGHTDIR}"
    if have vcsh ; then
        dim "revision:  $(vcsh "${NIGHT_DOTFILES_VCSH_NAME}" log --oneline -1 2>/dev/null || echo '?')"
    fi
else
    err "dotfiles not installed at ${NIGHTDIR}"
    note_problem
fi

#: A shell that hangs is worse than one that errors, so bound this hard.
if have zsh ; then
    t0=$(date +%s)
    if timeout -k 5 120 zsh -ic 'exit 0' >/dev/null 2>&1 ; then
        dim "interactive zsh: loads in $(( $(date +%s) - t0 ))s"
    else
        err "interactive zsh did not load cleanly within 120s"
        note_problem
    fi
fi

##
log "python / conda"
if have micromamba ; then
    dim "root prefix: ${MAMBA_ROOT_PREFIX:-unset}"
    micromamba env list 2>/dev/null | sed -n '3,12p' | while read -r line ; do
        [ -n "${line}" ] && dim "  ${line}"
    done
fi

##
log "emacs / doom"
if have emacs ; then
    dim "$(emacs --version 2>&1 | head -1)"
    nc="$(emacs --batch --eval '(princ (if (native-comp-available-p) "yes" "no"))' 2>/dev/null)"
    if [ "${nc}" = "yes" ] ; then
        dim "native-comp: yes"
    else
        warn "native-comp: no (Doom will be slow)"
        note_problem
    fi
    dim "DOOMLOCALDIR: ${DOOMLOCALDIR:-unset}"
    [ -d "${HOME}/.emacs.d" ] && dim "doom: $(git -C "${HOME}/.emacs.d" log --oneline -1 2>/dev/null || echo '?')"
    [ -e "${HOME}/.doom.d" ] && dim "config: $(git -C "${HOME}/doom.d" log --oneline -1 2>/dev/null || echo '?')"
fi

##
if [ "${problems}" -eq 0 ] ; then
    ok "no problems found"
else
    warn "${problems} problem(s) found"
fi
exit 0
