#!/usr/bin/env zsh
# Hermetic regression tests for tmux ancestry recovery used by agent-done.
# Run with: zsh -f this-file

setopt errexit nounset pipefail

typeset -gr agent_done_detection_root="${0:A:h:h:h}"
typeset -g agent_done_detection_tmp
agent_done_detection_tmp="$(command mktemp -d "${TMPDIR:-/tmp}/agent-done-detection.XXXXXX")"

function agent-done-detection-cleanup {
    if [[ -n "${agent_done_detection_tmp:-}" && -d "${agent_done_detection_tmp}" &&
        "${agent_done_detection_tmp:t}" == agent-done-detection.* ]] ; then
        command rm -rf -- "${agent_done_detection_tmp}"
    fi
}
trap agent-done-detection-cleanup EXIT

typeset -g agent_done_detection_bin="${agent_done_detection_tmp}/bin"
typeset -g agent_done_detection_ps="${agent_done_detection_tmp}/ps"
typeset -g agent_done_detection_panes="${agent_done_detection_tmp}/panes"
typeset -g agent_done_detection_calls="${agent_done_detection_tmp}/calls"
typeset -g agent_done_detection_tmux_calls="${agent_done_detection_tmp}/tmux-calls"
command mkdir -p -- "${agent_done_detection_bin}"

command cp /dev/null "${agent_done_detection_calls}"
command cp /dev/null "${agent_done_detection_tmux_calls}"

# The fake commands consume plain-text fixtures. Nothing in this test talks to
# the live tmux server or sends a signal.
command cp /dev/null "${agent_done_detection_bin}/ps"
command cp /dev/null "${agent_done_detection_bin}/tmux"
command cp /dev/null "${agent_done_detection_bin}/kill"
command chmod +x "${agent_done_detection_bin}/"{ps,tmux,kill}

function agent-done-detection-write-fakes {
    print -r -- '#!/bin/sh' >| "${agent_done_detection_bin}/ps"
    print -r -- 'pid=' >> "${agent_done_detection_bin}/ps"
    print -r -- 'for arg do pid=$arg; done' >> "${agent_done_detection_bin}/ps"
    print -r -- 'row=$(command awk -F "|" -v pid="$pid" '\''$1 == pid { print $2 " " $3; found=1; exit } END { if (!found) exit 1 }'\'' "$AGENT_DONE_DETECTION_PS") || exit 1' >> "${agent_done_detection_bin}/ps"
    print -r -- 'printf "%s\n" "$row"' >> "${agent_done_detection_bin}/ps"

    print -r -- '#!/bin/sh' >| "${agent_done_detection_bin}/tmux"
    print -r -- 'socket=' >> "${agent_done_detection_bin}/tmux"
    print -r -- 'if [ "$1" = -S ]; then socket=$2; shift 2; fi' >> "${agent_done_detection_bin}/tmux"
    print -r -- 'case "$1" in' >> "${agent_done_detection_bin}/tmux"
    print -r -- '  list-panes) [ ! -e "$AGENT_DONE_DETECTION_PANES.fail" ] || exit 1; command cat "$AGENT_DONE_DETECTION_PANES" ;;' >> "${agent_done_detection_bin}/tmux"
    print -r -- '  display-message) case "$*" in *pane_tty*) printf "%s\n" /dev/ttys999 ;; *pane_current_path*) printf "%s\n" /outer/project ;; *socket_path*) [ -z "$AGENT_DONE_DETECTION_NO_SOCKET" ] && printf "%s\n" "/tmp/tmux socket" ;; *) exit 1 ;; esac ;;' >> "${agent_done_detection_bin}/tmux"
    print -r -- '  respawn-pane) printf "socket=%s command=%s pane=%s\n" "$socket" "$1" "$4" >> "$AGENT_DONE_DETECTION_TMUX_CALLS" ;;' >> "${agent_done_detection_bin}/tmux"
    print -r -- '  *) exit 1 ;;' >> "${agent_done_detection_bin}/tmux"
    print -r -- 'esac' >> "${agent_done_detection_bin}/tmux"

    print -r -- '#!/bin/sh' >| "${agent_done_detection_bin}/kill"
    print -r -- 'printf "kill %s\n" "$*" >> "$AGENT_DONE_DETECTION_CALLS"' >> "${agent_done_detection_bin}/kill"
    print -r -- 'exit 99' >> "${agent_done_detection_bin}/kill"
}
agent-done-detection-write-fakes

typeset -gx AGENT_DONE_DETECTION_PS="${agent_done_detection_ps}"
typeset -gx AGENT_DONE_DETECTION_PANES="${agent_done_detection_panes}"
typeset -gx AGENT_DONE_DETECTION_CALLS="${agent_done_detection_calls}"
typeset -gx AGENT_DONE_DETECTION_TMUX_CALLS="${agent_done_detection_tmux_calls}"
typeset -gx AGENT_DONE_DETECTION_NO_SOCKET=''
typeset -gx PATH="${agent_done_detection_bin}:${PATH}"
# `command kill` can still select zsh's builtin. Disable it so even a
# regression accidentally crossing the dry-run/refusal boundary stays safe.
disable kill

unset night_basic_plugin_loaded_p
source "${agent_done_detection_root}/zshlang/basic/basic.plugin.zsh"

function agent-done-detection-fail {
    print -ru2 -- "FAIL: $*"
    return 1
}

function agent-done-detection-fixture {
    print -r -- '900|800|/usr/local/bin/script' >| "${agent_done_detection_ps}"
    print -r -- '800|700|/usr/bin/python3' >> "${agent_done_detection_ps}"
    print -r -- '700|600|-zsh' >> "${agent_done_detection_ps}"
    print -r -- '600|1|tmux: server' >> "${agent_done_detection_ps}"
    print -r -- $'%3\t700\t0' >| "${agent_done_detection_panes}"
    command rm -f -- "${agent_done_detection_panes}.fail"
}

function agent-done-detection-expect {
    local description="${1}" expected_status="${2}" expected_output="${3}"
    shift 3
    local output='' actual_status=0
    output="$( { unset TMUX TMUX_PANE; h-tmux-pane-of-pid "$@"; } 2>/dev/null)" || actual_status=$?
    (( actual_status == expected_status )) ||
        agent-done-detection-fail "${description}: expected status ${expected_status}, got ${actual_status}"
    [[ "${output}" == "${expected_output}" ]] ||
        agent-done-detection-fail "${description}: expected ${(qqq)expected_output}, got ${(qqq)output}"
}

agent-done-detection-fixture
agent-done-detection-expect 'proxy ancestry without environment hints' 0 %3 900

typeset output='' actual_status=0
output="$(TMUX_PANE=%3 TMUX=/tmp/custom,1,0 h-tmux-pane-of-pid 900 2>/dev/null)" || actual_status=$?
(( actual_status == 0 )) && [[ "${output}" == %3 ]] || agent-done-detection-fail 'valid pane hint'
actual_status=0
TMUX_PANE=%8 TMUX=/tmp/custom,1,0 h-tmux-pane-of-pid 900 >/dev/null 2>&1 || actual_status=$?
(( actual_status == 2 )) || agent-done-detection-fail 'stale pane hint should be uncertain'
actual_status=0
TMUX_PANE=malformed TMUX='' h-tmux-pane-of-pid 900 >/dev/null 2>&1 || actual_status=$?
(( actual_status == 2 )) || agent-done-detection-fail 'malformed pane hint should be uncertain'

print -r -- '900|2|zsh' >| "${agent_done_detection_ps}"
print -r -- '2|1|launchd' >> "${agent_done_detection_ps}"
: >| "${agent_done_detection_panes}"
agent-done-detection-expect 'outside tmux' 1 '' 900
command touch "${agent_done_detection_panes}.fail"
agent-done-detection-expect 'unreachable server outside tmux' 1 '' 900
actual_status=0
TMUX='' TMUX_PANE=%3 h-tmux-pane-of-pid 900 >/dev/null 2>&1 || actual_status=$?
(( actual_status == 2 )) || agent-done-detection-fail 'unreachable hinted server should be uncertain'

agent-done-detection-fixture
command touch "${agent_done_detection_panes}.fail"
agent-done-detection-expect 'unreachable server with tmux ancestry and no hints' 2 '' 900
agent-done-detection-fixture
: >| "${agent_done_detection_panes}"
actual_status=0
TMUX=/tmp/custom,1,0 TMUX_PANE='' h-tmux-pane-of-pid 900 >/dev/null 2>&1 || actual_status=$?
(( actual_status == 2 )) || agent-done-detection-fail 'tmux ancestry without a matching pane should be uncertain'

print -r -- $'%3\t700\t1' >| "${agent_done_detection_panes}"
actual_status=0
TMUX='' TMUX_PANE='' h-tmux-pane-of-pid 900 >/dev/null 2>&1 || actual_status=$?
(( actual_status == 2 )) || agent-done-detection-fail 'dead owning pane should be uncertain'
print -r -- $'%3\t700\t0\n%4\t800\t0' >| "${agent_done_detection_panes}"
actual_status=0
h-tmux-pane-of-pid 900 >/dev/null 2>&1 || actual_status=$?
(( actual_status == 2 )) || agent-done-detection-fail 'ambiguous live panes should be uncertain'

agent-done-detection-fixture
print -r -- '900|bad|zsh' >| "${agent_done_detection_ps}"
agent-done-detection-expect 'malformed process row' 2 '' 900
: >| "${agent_done_detection_ps}"
agent-done-detection-expect 'missing process' 2 '' 900
print -r -- '900|800|zsh' >| "${agent_done_detection_ps}"
print -r -- '800|900|proxy' >> "${agent_done_detection_ps}"
agent-done-detection-expect 'cyclic process ancestry' 2 '' 900
agent-done-detection-expect 'malformed pid' 2 '' nope

# Exercise agent-done's dry-run boundary with fake adapters and report writers.
function aliasfn { :; }
function ec { print -r -- "$@"; }
function ecerr { print -ru2 -- "$@"; }
function assert-args { return 0; }
typeset -g NIGHTDIR="${agent_done_detection_root}"
source "${agent_done_detection_root}/zshlang/auto-load/others/agent-done.zsh"
typeset -g agent_done_agent=codex agent_done_pid=900 agent_done_tty='' agent_done_cwd=''
typeset -g agent_done_resume_cmd=''
typeset -gx TMUX='' TMUX_PANE=''
function h-agent-done-id { return 1; }
function h-agent-done-pid { print -r -- 900; }
function h-agent-session-dir { print -r -- "${3}"; }
function h-agent-done-dir { print -r -- "${agent_done_detection_tmp}/reports"; }
function h-agent-done-report { command touch -- "${7}"; }
function h-agent-done-pane-script { command touch -- "${4}"; }
function agent-auto-continue-off { print -r -- auto-continue >> "${agent_done_detection_calls}"; }

agent-done-detection-fixture
output="$(agent-done --dry-run finished </dev/null)"
[[ "${output}" == *'pane=%3 tty=/dev/ttys999 socket=/tmp/tmux socket'* ]] ||
    agent-done-detection-fail 'dry-run should report the recovered outer pane tty and socket'
[[ ! -s "${agent_done_detection_calls}" ]] ||
    agent-done-detection-fail 'dry-run must not unregister auto-continue or send a signal'

: >| "${agent_done_detection_panes}"
actual_status=0
agent-done --dry-run uncertain </dev/null >/dev/null 2>&1 || actual_status=$?
(( actual_status == 2 )) || agent-done-detection-fail 'uncertain ownership should make agent-done refuse'
[[ ! -s "${agent_done_detection_calls}" ]] ||
    agent-done-detection-fail 'ownership refusal must happen before unregister or kill'

agent-done-detection-fixture
typeset -gx AGENT_DONE_DETECTION_NO_SOCKET=y
actual_status=0
agent-done --dry-run missing-socket </dev/null >/dev/null 2>&1 || actual_status=$?
(( actual_status == 2 )) || agent-done-detection-fail 'missing pane socket metadata should refuse'
typeset -gx AGENT_DONE_DETECTION_NO_SOCKET=''

typeset watcher_report="${agent_done_detection_tmp}/watcher-report"
print -r -- report >| "${watcher_report}"
h-agent-done-watch '' %3 "${watcher_report}" '' '' '/tmp/tmux socket'
[[ "$(<"${agent_done_detection_tmux_calls}")" == 'socket=/tmp/tmux socket command=respawn-pane pane=%3' ]] ||
    agent-done-detection-fail 'watcher should preserve the exact custom socket and pane'
[[ ! -s "${agent_done_detection_calls}" ]] ||
    agent-done-detection-fail 'watcher fixture with an empty pid must not invoke kill'

# Simulate the owning pane disappearing during report creation. Exercise the
# non-dry-run recheck, protected by fake commands and the disabled kill builtin.
agent-done-detection-fixture
function h-agent-done-report {
    command touch -- "${7}"
    : >| "${agent_done_detection_panes}"
}
actual_status=0
agent-done ownership-changed </dev/null >/dev/null 2>&1 || actual_status=$?
(( actual_status == 2 )) || agent-done-detection-fail 'changed ownership should refuse at the final recheck'
[[ ! -s "${agent_done_detection_calls}" ]] ||
    agent-done-detection-fail 'changed ownership must refuse before unregister or kill'

print -r -- 'ok: agent-done tmux ownership detection'
