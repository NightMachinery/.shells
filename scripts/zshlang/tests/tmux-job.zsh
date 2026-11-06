#!/usr/bin/env zsh
# tmux-job-* must tell a missing job from an unreachable tmux server.
# A fake `tmux' on PATH plays each server; no real tmux is touched.
setopt errexit pipefail typesetsilent
typeset -gr tmux_job_test_root="${${(%):-%x}:A:h:h:h}"
typeset -g tmux_job_test_tmp="$(command mktemp -d "${TMPDIR:-/tmp}/tmux-job-tests.XXXXXX")"
trap 'command rm -rf -- "${tmux_job_test_tmp}"' EXIT
unset night_basic_plugin_loaded_p
source "${tmux_job_test_root}/zshlang/basic/basic.plugin.zsh"
source "${tmux_job_test_root}/zshlang/auto-load/others/tmux.zsh"
unalias ecerr ecgray 2>/dev/null || true
function ecerr { print -ru2 -- "$@"; }
function ecgray { print -ru2 -- "$@"; }
function tmuxnewsh2 { print -r -- "started $*" >> "${tmux_job_test_tmp}/calls"; }
function tmux-session-processes-kill { print -r -- "killed $*" >> "${tmux_job_test_tmp}/calls"; }

command mkdir -- "${tmux_job_test_tmp}/bin"
cat > "${tmux_job_test_tmp}/bin/tmux" <<'SH'
#!/bin/sh
#: TMUX_FAKE: denied (sandboxed socket), none (no server), live (job running)
case "${TMUX_FAKE}" in
    denied) echo "error connecting to /tmp/tmux-501/default (Operation not permitted)" >&2; exit 1 ;;
    none) echo "error connecting to /tmp/tmux-501/default (No such file or directory)" >&2; exit 1 ;;
    live)
        case "$1" in
            has-session|list-sessions) exit 0 ;;
            list-panes) case "$*" in *pane_dead*) echo 0 ;; *) echo 4242 ;; esac; exit 0 ;;
        esac ;;
esac
exit 0
SH
command chmod 700 "${tmux_job_test_tmp}/bin/tmux"
path=("${tmux_job_test_tmp}/bin" "${path[@]}")

typeset -gi tmux_job_test_failed=0
function check {
    local label="$1" want_rc="$2" want_out="$3" ; shift 3
    local out rc=0
    out="$("$@" 2>&1)" || rc=$?
    if [[ "${rc}" != "${want_rc}" || "${out}" != *"${want_out}"* ]] ; then
        print -r -- "FAIL ${label}: rc=${rc} (want ${want_rc}) out=${out}"
        tmux_job_test_failed=1
    else
        print -r -- "ok   ${label}"
    fi
}

export TMUX_FAKE=denied
check 'denied: status is UNKNOWN'   2 'elevated access'          tmux-job-status job
check 'denied: running-p is 2'      2 'cannot be reached'       tmux-job-running-p job
check 'denied: start refuses'       2 'not starting it'          tmux-job-start job sleep 1
check 'denied: stop refuses'        2 'cannot be reached'       tmux-job-stop job
export TMUX_FAKE=none
check 'none: status is not running' 1 'not running'              tmux-job-status job
check 'none: stop is a no-op'       0 'nothing to stop'          tmux-job-stop job
check 'none: start starts'          0 'started job'              tmux-job-start job sleep 1
export TMUX_FAKE=live
check 'live: status is running'     0 'running (pane pid 4242)'  tmux-job-status job
check 'live: start refuses'         1 'already running'          tmux-job-start job sleep 1
exit "${tmux_job_test_failed}"
