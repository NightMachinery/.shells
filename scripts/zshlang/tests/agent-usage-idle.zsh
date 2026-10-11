#!/usr/bin/env zsh
# The armed resume's per-target idle check. Fixture-only: nothing is typed,
# sent or notified, and Hammerspoon is never asked.
# Run with: zsh -f zshlang/tests/agent-usage-idle.zsh

setopt errexit pipefail typesetsilent

typeset -gr idle_test_root="${0:A:h:h:h}"
typeset -g idle_test_tmp
idle_test_tmp="$(command mktemp -d "${TMPDIR:-/tmp}/agent-usage-idle-tests.XXXXXX")"

function idle-test-cleanup {
    if [[ -n "${idle_test_tmp:-}" && -d "${idle_test_tmp}" &&
        "${idle_test_tmp:t}" == agent-usage-idle-tests.* ]] ; then
        command rm -rf -- "${idle_test_tmp}"
    fi
}
trap idle-test-cleanup EXIT

unset night_basic_plugin_loaded_p
source "${idle_test_root}/zshlang/basic/basic.plugin.zsh"
function aliasfn { :; }
function aliasfnq { :; }
source "${idle_test_root}/zshlang/auto-load/others/agent-usage.zsh"
setopt nounset

typeset -g log="${idle_test_tmp}/log" idle_now=5 locked_p=n
function h-agent-usage-idle-s {
    print -r -- idle-read >> "${log}"
    [[ "${idle_now}" == fail ]] && return 1
    print -r -- "${idle_now}"
}
function h-agent-usage-screen-locked-p { [[ "${locked_p}" == y ]]; }
function h-agent-usage-continue-send { print -r -- "sent ${1}" >> "${log}"; }
function h-agent-usage-arm-notify { print -r -- "notify ${1}" >> "${log}"; }
function h-agent-usage-arm-log { :; }
function seconds-fmt-short { print -r -- "${1}s"; }

function expect-eq {
    if [[ "${1}" != "${2}" ]] ; then
        print -ru2 -- "FAIL: ${3}"
        print -ru2 -- "  want: ${(q+)2}"
        print -ru2 -- "  got:  ${(q+)1}"
        exit 1
    fi
}

function fire {
    #: Usage: fire <targets...>; prints the log.
    : > "${log}"
    agent_usage_arm_action=continue agent_usage_arm_targets="${*}" \
        h-agent-usage-arm-fire job 'Reset'
    command cat -- "${log}"
}

# Defaults: only frontmost waits, and the idle time is read once.
out="$(fire tmux:%1 kitty:3 codex:t paseo:p claude-bg:b frontmost)"
expect-eq "${out}" "idle-read"$'\n'"sent tmux:%1"$'\n'"sent kitty:3"$'\n'"sent codex:t"$'\n'"sent paseo:p"$'\n'"sent claude-bg:b"$'\n'"notify Reset -- resumed tmux:%1, kitty:3, codex:t, paseo:p, claude-bg:b -- not resuming frontmost (you were at the keyboard (5s idle, needs 600s))" \
    'defaults hold back only frontmost'

# With every target at 0, Hammerspoon is not asked, so it being down is fine.
idle_now=fail
out="$(fire tmux:%1 paseo:p)"
expect-eq "${out}" "sent tmux:%1"$'\n'"sent paseo:p"$'\n'"notify Reset -- resumed tmux:%1, paseo:p" \
    'no idle read when nothing needs it'

# A kind given a threshold waits for it; one that cannot be read holds back.
out="$(agent_usage_continue_idle_min_s_tmux=600 fire tmux:%1 paseo:p)"
expect-eq "${out}" "idle-read"$'\n'"sent paseo:p"$'\n'"notify Reset -- resumed paseo:p -- not resuming tmux:%1 (could not read the idle time)" \
    'unreadable idle time holds back a waiting kind'
idle_now=900
out="$(agent_usage_continue_idle_min_s_tmux=600 fire tmux:%1)"
expect-eq "${out}" "idle-read"$'\n'"sent tmux:%1"$'\n'"notify Reset -- resumed tmux:%1" 'idle long enough'

# frontmost at 0 skips the idle check but never types on a locked screen.
idle_now=5 locked_p=y
out="$(agent_usage_continue_idle_min_s_frontmost=0 fire frontmost)"
expect-eq "${out}" "notify Reset -- not resuming frontmost (the screen is locked)" 'locked screen holds frontmost'
locked_p=n
out="$(agent_usage_continue_idle_min_s_frontmost=0 fire frontmost)"
expect-eq "${out}" "sent frontmost"$'\n'"notify Reset -- resumed frontmost" 'frontmost at 0'

# A bad setting fails safe; an unknown kind falls back to the general value.
out="$(agent_usage_continue_idle_min_s_paseo=soon fire paseo:p)"
expect-eq "${out}" "notify Reset -- not resuming paseo:p (its idle setting is not a whole number of seconds: soon)" 'bad setting'
expect-eq "$(h-agent-usage-continue-idle-min-s other:1)" 600 'unknown kind falls back'
expect-eq "$(agent_usage_continue_idle_min_s=42 h-agent-usage-continue-idle-min-s frontmost)" 42 'frontmost follows the general value'

print -r -- 'ok: per-target idle check'
