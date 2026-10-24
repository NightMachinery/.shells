#!/usr/bin/env zsh
# The measured reset wait (agent-usage-reset-wait). Fixture-only: readings are
# scripted, sleep is a no-op, and nothing is sent, notified or started in tmux.
# Run with: zsh -f zshlang/tests/agent-usage-reset-wait.zsh

setopt errexit pipefail typesetsilent

typeset -gr rw_test_root="${0:A:h:h:h}"
typeset -g rw_test_tmp
rw_test_tmp="$(command mktemp -d "${TMPDIR:-/tmp}/agent-usage-reset-wait-tests.XXXXXX")"

function rw-test-cleanup {
    if [[ -n "${rw_test_tmp:-}" && -d "${rw_test_tmp}" &&
        "${rw_test_tmp:t}" == agent-usage-reset-wait-tests.* ]] ; then
        command rm -rf -- "${rw_test_tmp}"
    fi
}
trap rw-test-cleanup EXIT

unset night_basic_plugin_loaded_p PASEO_AGENT_ID
source "${rw_test_root}/zshlang/basic/basic.plugin.zsh"
function aliasfn { :; }
function aliasfnq { :; }
source "${rw_test_root}/zshlang/auto-load/others/agent-usage.zsh"
source "${rw_test_root}/zshlang/auto-load/others/codex.zsh"
source "${rw_test_root}/zshlang/auto-load/others/claude.zsh"

typeset -g rw_log="${rw_test_tmp}/log" counter="${rw_test_tmp}/counter"
#: Should anything slip past the stubs, it lands here, not in the owner's log.
typeset -g agent_usage_arm_log="${rw_test_tmp}/arm.log"
typeset -g uuid='00000000-0000-4000-8000-000000000001'
typeset -ga readings
typeset -gi send_fail_n=0

#: One scripted reading per call. The reader runs in a command substitution,
#: so the position lives in a file rather than a variable.
function h-codex-status-gauges {
    integer i=$(( $(<"${counter}") + 1 ))
    print -r -- "${i}" > "${counter}"
    local r="${readings[i]:-${readings[-1]}}"
    [[ "${r}" == FAIL ]] && return 1
    print -r -- "${r}"
}
function sleep {
    #: A wait that never fires would otherwise spin forever on a no-op sleep.
    (( $(<"${counter}") < 30 )) || { print -ru2 -- 'FAIL: the wait did not end'; exit 1 }
}
function h-agent-usage-continue-send {
    if (( send_fail_n > 0 )) ; then
        send_fail_n=$(( send_fail_n - 1 ))
        print -r -- "unreachable ${1}" >> "${rw_log}"
        return 1
    fi
    print -r -- "sent ${1}: ${agent_usage_continue_text}" >> "${rw_log}"
}
function ecgray { print -ru2 -- "$@"; }
function ecerr { print -ru2 -- "$@"; }
function silent { "$@" >/dev/null 2>&1; }
function h-agent-usage-arm-notify { print -r -- "notify ${1}" >> "${rw_log}"; }
function h-agent-usage-arm-log { print -r -- "log ${1}" >> "${rw_log}"; }
function seconds-fmt-short { print -r -- "${1}s"; }
function date-unix-to-3339 { print -r -- "@${1}"; }
function dur2sec { [[ "${1}" == <-> ]] && print -r -- "${1}"; }
function h-tmux-session-name-sanitize { print -r -- "${1}"; }
function tmux-job-running-p { return 1; }
function tmuxnewsh2 { print -r -- "tmuxnewsh2 ${(j: :)${(q-)@}}" >> "${rw_log}"; }
function tmux { print -r -- "tmux ${(j: :)${(q-)@}}" >> "${rw_log}"; }

function expect-eq {
    if [[ "${1}" != "${2}" ]] ; then
        print -ru2 -- "FAIL: ${3}"
        print -ru2 -- "  want: ${(q+)2}"
        print -ru2 -- "  got:  ${(q+)1}"
        exit 1
    fi
}
function expect-in {
    if [[ "${1}" != *"${2}"* ]] ; then
        print -ru2 -- "FAIL: ${3}"
        print -ru2 -- "  want to find: ${(q+)2}"
        print -ru2 -- "  in:           ${(q+)1}"
        exit 1
    fi
}

function log-count {
    #: Usage: log-count <glob>; how many log lines match.
    local -a lines hits
    lines=( "${(@f)$(<"${rw_log}")}" )
    hits=( "${(@M)lines:#${~1}}" )
    print -r -- "${#hits}"
}

function run {
    #: Usage: run <expected-status> <args...>; resets the log and the script.
    local want="${1}" got=0
    shift
    : > "${rw_log}"
    print -r -- 0 > "${counter}"
    agent-usage-reset-wait "$@" > "${rw_test_tmp}/out" 2> "${rw_test_tmp}/err" || got=$?
    expect-eq "${got}" "${want}" "exit status of: $*  (stderr: $(<"${rw_test_tmp}/err"))"
}

# Below a threshold: fires once the watched window reads under it.
readings=( $'auth.7d\t95\t1000' $'auth.7d\t95\t1000' $'auth.7d\t40\t1000' )
run 0 --fg --below 60 --to "paseo:${uuid}" --note 'Resume.' codex
expect-in "$(<"${rw_log}")" "sent paseo:${uuid}: QUOTA RESET (measured by agent-usage-reset-wait): codex auth.7d 95% -> 40%. Resume." 'below threshold fires'
expect-eq "$(<"${counter}")" 3 'below: one arm reading and two polls'

# The default watched set is what is at or above the threshold now.
readings=( $'auth.5h\t10\t1000\nauth.7d\t95\t2000' $'auth.5h\t70\t1000\nauth.7d\t50\t2000' )
run 0 --fg --below 60 codex
expect-in "$(<"${rw_test_tmp}/err")" 'auth.7d 95% used, resets @2000 (watching)' 'watched window is marked'
expect-in "$(<"${rw_test_tmp}/out")" 'codex auth.7d 95% -> 50%.' 'a window that was not high is not watched'

# Already below: nothing to wait for, nothing armed.
readings=( $'auth.7d\t30\t1000' )
run 0 --below 60 --to "paseo:${uuid}" codex
expect-eq "$(<"${rw_log}")" '' 'already below arms nothing'

# No threshold: a large drop fires; a small one does not.
readings=( $'auth.7d\t95\t1000' $'auth.7d\t80\t1000' $'auth.7d\t2\t1000' )
run 0 --fg codex
expect-in "$(<"${rw_test_tmp}/out")" 'auth.7d 95% -> 2% (dropped)' 'large drop fires'
expect-eq "$(<"${counter}")" 3 'a 15 point drop is not a reset'

# No threshold: the reset time moving later while in use fires; jitter, and a
# window nobody used, do not.
readings=( $'a.5h\t0\t1000\nb.5h\t30\t1000' $'a.5h\t0\t9000\nb.5h\t30\t1030' $'a.5h\t0\t9999\nb.5h\t31\t20000' )
run 0 --fg codex
expect-in "$(<"${rw_test_tmp}/out")" 'b.5h 30% -> 31% (window rolled over)' 'rollover fires'
[[ "$(<"${rw_test_tmp}/out")" != *'a.5h 0%'* ]] || { print -ru2 -- 'FAIL: an unused window fired'; exit 1 }

# Reader failures warn once per run of failures, then the wait goes on.
readings=( $'auth.7d\t95\t1000' FAIL FAIL FAIL $'auth.7d\t95\t1000' $'auth.7d\t10\t1000' )
agent_usage_reset_wait_fail_warn_n=2 run 0 --fg --below 60 --to "paseo:${uuid}" codex
expect-eq "$(log-count "sent *WARNING*")" 1 'one warning for three failures'
expect-in "$(<"${rw_log}")" 'reader failed 2 times in a row' 'warning names the count'
expect-in "$(<"${rw_log}")" 'auth.7d 95% -> 10%' 'fires after failures'

# A target that cannot be reached is retried until it is.
send_fail_n=2
readings=( $'auth.7d\t95\t1000' $'auth.7d\t5\t1000' )
run 0 --fg --below 60 --to "paseo:${uuid}" codex
expect-eq "$(log-count "unreachable *")" 2 'two failed attempts'
expect-in "$(<"${rw_log}")" "sent paseo:${uuid}: QUOTA RESET" 'delivered on the third'

# Retries stop after retry_s.
send_fail_n=99
readings=( $'auth.7d\t95\t1000' $'auth.7d\t5\t1000' )
agent_usage_reset_wait_retry_s=0 run 0 --fg --below 60 --to "paseo:${uuid}" codex
expect-in "$(<"${rw_log}")" "gave up on paseo:${uuid}" 'gives up'
send_fail_n=0

# A dry run sends nothing.
readings=( $'auth.7d\t95\t1000' $'auth.7d\t5\t1000' )
run 0 --fg --dry-run --poll 1 --below 60 --to "paseo:${uuid}" codex
[[ "$(<"${rw_log}")" != *sent* ]] || { print -ru2 -- 'FAIL: a dry run sent'; exit 1 }
expect-in "$(<"${rw_test_tmp}/out")" "dry run, not sent to paseo:${uuid}" 'dry run says so'

# Bad input is refused before anything is read.
run 2 --below 0 codex
run 2 --below 60 --to paseo:not-a-uuid codex
run 2 --below 60 --to "nowhere:1" codex
run 2 --below 60 --to self codex
run 2 --poll 5 codex
run 2 --below 60 gemini
expect-eq "$(<"${counter}")" 0 'nothing was read for bad input'
readings=( $'auth.7d\t95\t1000' )
run 1 --gauge 'nope.*' codex

# Detached: one tmux session, from home, carrying the watched windows and the
# arm-time reading; the condition is recorded on the session.
readings=( $'auth.5h\t10\t1000\nauth.7d\t95\t2000' )
PASEO_AGENT_ID="${uuid}" run 0 --below 60 --to self --name coord codex
out="$(<"${rw_log}")"
expect-in "${out}" 'tmuxnewsh2 agent-usage-reset-wait-coord ' 'session name'
expect-in "${out}" "agent_usage_arm_targets=paseo:${uuid}" 'self resolves to this agent'
expect-in "${out}" 'agent_usage_reset_wait_watched=auth.7d ' 'watched windows travel'
expect-in "${out}" 'agent_usage_reset_wait_baseline=auth.7d=95@2000 ' 'baseline travels'
expect-in "${out}" 'h-agent-usage-reset-wait-loop codex agent-usage-reset-wait-coord' 'the loop is the body'
expect-in "${out}" "tmux set-option -t agent-usage-reset-wait-coord @agent_usage_reset_condition 'codex auth.7d all below 60%'" 'condition recorded'

# The readers: malformed rows are dropped, and the jq keeps only numbers.
function h-claude-code-usage-gauges { print -r -- $'ok.5h\t5\t1\nbad\t5\t1\nx.7d\t101\t1\ny.7d\t5\tsoon' }
h-agent-usage-reset-read claude
expect-eq "${reply[*]}" 'ok.5h 5 1' 'malformed rows dropped'

unfunction h-codex-status-gauges h-claude-code-usage-gauges
source "${rw_test_root}/zshlang/auto-load/others/codex.zsh"
source "${rw_test_root}/zshlang/auto-load/others/claude.zsh"
function codex-status {
    print -r -- '{"authFiles":[{"ok":true,"alias":"one","email":"x@example.test","quota":{"windows":[
      {"key":"primary","windowDurationMins":300,"usedPercent":12.4,"resetsAt":1000.5},
      {"key":"secondary","windowDurationMins":10080,"usedPercent":95,"resetsAt":2000},
      {"key":"primary","windowDurationMins":60,"usedPercent":1,"resetsAt":null}]}},
      {"ok":false,"alias":"two"}]}'
    return 1
}
expect-eq "$(h-codex-status-gauges)" $'one.5h\t12\t1000\none.7d\t95\t2000\none.primary\t1\t-' 'codex reader'
function claude-code-usage-all {
    print -r -- '[{"profile":"default","windows":[{"key":"session","utilization_percent":22.0,"resets_at":1000.9},
      {"key":"weekly_all","utilization_percent":91,"resets_at":2000}]},
      {"profile":"work","error":"Login required"},
      {"profile":"old","cache":{"stale_reason":"fetch failed"},"windows":[{"key":"session","utilization_percent":1,"resets_at":1}]}]'
    return 1
}
expect-eq "$(h-claude-code-usage-gauges)" $'default.5h\t22\t1000\ndefault.7d\t91\t2000' 'claude reader skips errors and stale caches'

print -r -- 'ok: measured reset wait'
