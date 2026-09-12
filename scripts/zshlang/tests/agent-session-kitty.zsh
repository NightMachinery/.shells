#!/usr/bin/env zsh
# Regression tests for resolving the transcript shown by a kitty window.
# Run with: zsh -f zshlang/tests/agent-session-kitty.zsh

setopt errexit nounset pipefail typesetsilent

typeset -gr kitty_transcript_test_root="${0:A:h:h:h}"
typeset -g kitty_transcript_test_tmp
kitty_transcript_test_tmp="$(command mktemp -d "${TMPDIR:-/tmp}/kitty-transcript-tests.XXXXXX")"

function kitty-transcript-test-cleanup {
    if [[ -n "${kitty_transcript_test_tmp:-}" && -d "${kitty_transcript_test_tmp}" &&
        "${kitty_transcript_test_tmp:t}" == kitty-transcript-tests.* ]] ; then
        command rm -rf -- "${kitty_transcript_test_tmp}"
    fi
}
trap kitty-transcript-test-cleanup EXIT

# Load the public-safe basics and the production function definitions without
# loading the personal shell. aliasfn only handles top-level convenience aliases
# in agent-session.zsh; none are needed by these tests.
unset night_basic_plugin_loaded_p
source "${kitty_transcript_test_root}/zshlang/basic/basic.plugin.zsh"
function aliasfn { :; }
source "${kitty_transcript_test_root}/zshlang/auto-load/others/agent-session.zsh"

typeset -g kitty_transcript_test_window_row=''
typeset -g kitty_transcript_test_clients=''
typeset -g kitty_transcript_test_identities=''
typeset -g kitty_transcript_test_live=''
typeset -g kitty_transcript_test_registry=''
typeset -g kitty_transcript_test_live_calls="${kitty_transcript_test_tmp}/live-calls"

# Everything below is fixture-backed. No helper reaches live kitty, tmux, or
# process state.
function h-agent-session-window-row {
    print -r -- "${kitty_transcript_test_window_row}"
}

function h-agent-session-tmux-client-session {
    local wanted="${1}" fixture_row
    for fixture_row in ${(f)kitty_transcript_test_clients} ; do
        if [[ "${fixture_row%%$'\t'*}" == "${wanted}" ]] ; then
            print -r -- "${fixture_row#*$'\t'}"
            return 0
        fi
    done
    return 1
}

function h-agent-session-tmux-identity {
    local wanted="${1}" fixture_row
    for fixture_row in ${(f)kitty_transcript_test_identities} ; do
        if [[ "${fixture_row%%$'\t'*}" == "${wanted}" ]] ; then
            print -r -- "${fixture_row#*$'\t'}"
            return 0
        fi
    done
    return 1
}

function h-agent-session-live-list {
    print -r -- called >> "${kitty_transcript_test_live_calls}"
    print -r -- "${kitty_transcript_test_live}"
}

function h-agent-session-cmds-agent-p {
    return 1
}

function h-agent-session-registry-key {
    print -r -- "${1}-${2}"
}

function h-agent-session-registry-read {
    test -n "${kitty_transcript_test_registry}" &&
        test -e "${kitty_transcript_test_registry}" || return 1
    print -r -- "${kitty_transcript_test_registry}"
}

function kitty-transcript-test-fail {
    print -ru2 -- "FAIL: $*"
    return 1
}

function kitty-transcript-test-reset {
    kitty_transcript_test_window_row=''
    kitty_transcript_test_clients=''
    kitty_transcript_test_identities=''
    kitty_transcript_test_live=''
    kitty_transcript_test_registry=''
    : >| "${kitty_transcript_test_live_calls}"
    unset agent_session_live_list_cache
}

function kitty-transcript-test-live-row {
    local pid="${1}" id="${2}" name="${3}" transcript="${4}" tmux_name="${5}"
    local fixture_row="${pid}"$'\t'"${id}"$'\t'"${name}"$'\t/tmp/project\t'\
"${transcript}"$'\t'"${tmux_name}"$'\tworking\tinteractive'
    print -r -- "${fixture_row}"
}

function kitty-transcript-test-expect {
    local description="${1}" expected_status="${2}" expected_output="${3}"
    local output='' actual_status=0

    output="$(h-agent-session-of-kitty-window '{}' 7 900)" || actual_status=$?
    (( actual_status == expected_status )) ||
        kitty-transcript-test-fail \
            "${description}: expected status ${expected_status}, got ${actual_status} (output ${(qqq)output})"
    [[ "${output}" == "${expected_output}" ]] ||
        kitty-transcript-test-fail \
            "${description}: expected ${(qqq)expected_output}, got ${(qqq)output}"
}

typeset -g transcript_new="${kitty_transcript_test_tmp}/new.jsonl"
typeset -g transcript_old="${kitty_transcript_test_tmp}/old.jsonl"
typeset -g transcript_other="${kitty_transcript_test_tmp}/other.jsonl"
typeset -g transcript_identity="${kitty_transcript_test_tmp}/identity.jsonl"
typeset -g transcript_registry="${kitty_transcript_test_tmp}/registry.jsonl"
: >| "${transcript_new}"
: >| "${transcript_old}"
: >| "${transcript_other}"
: >| "${transcript_identity}"
: >| "${transcript_registry}"

# A numeric tmux target is a server-side id, not the current session name. The
# attached client pid says the session is now named new-name.
kitty-transcript-test-reset
kitty_transcript_test_window_row=$'7\tnumeric target\t491\ttmux attach -t $491'
kitty_transcript_test_clients=$'491\tnew-name'
kitty_transcript_test_live="$(kitty-transcript-test-live-row 710 session-new New "${transcript_new}" new-name)"
kitty-transcript-test-expect 'numeric tmux target uses the client actual session' 0 "${transcript_new}"

# A command line survives both rename-session and switch-client. Even if its
# stale name now belongs to another live session, it must not win over the
# session to which pid 492 is actually attached.
kitty-transcript-test-reset
kitty_transcript_test_window_row=$'7\tstale command\t492\ttmux attach -t old-name'
kitty_transcript_test_clients=$'492\tnew-name'
kitty_transcript_test_live="$(kitty-transcript-test-live-row 720 session-new New "${transcript_new}" new-name)"$'\n'\
"$(kitty-transcript-test-live-row 721 session-old Old "${transcript_old}" old-name)"
kitty-transcript-test-expect 'renamed or switched client ignores stale command target' 0 "${transcript_new}"

# @agent_session remains the cheapest and strongest path. It must return before
# constructing a live list.
kitty-transcript-test-reset
kitty_transcript_test_window_row=$'7\tidentity\t493\ttmux attach -t old-name'
kitty_transcript_test_clients=$'493\tnew-name'
kitty_transcript_test_identities="new-name"$'\t'"codex"$'\t'"session-identity"$'\t'"${transcript_identity}"
kitty_transcript_test_live="$(kitty-transcript-test-live-row 730 should-not-run Nope "${transcript_other}" new-name)"
kitty-transcript-test-expect 'tmux identity fast path' 0 "${transcript_identity}"
[[ ! -s "${kitty_transcript_test_live_calls}" ]] ||
    kitty-transcript-test-fail 'tmux identity fast path should not call the live-list helper'

# One tmux session can contain multiple live agents. The window does not reveal
# which pane is intended, so ambiguity must stop resolution before stale registry
# insurance is consulted.
kitty-transcript-test-reset
kitty_transcript_test_window_row=$'7\tambiguous tmux\t494\ttmux attach -t obsolete-name'
kitty_transcript_test_clients=$'494\tnew-name'
kitty_transcript_test_live="$(kitty-transcript-test-live-row 740 session-a A "${transcript_new}" new-name)"$'\n'\
"$(kitty-transcript-test-live-row 741 session-b B "${transcript_other}" new-name)"
kitty_transcript_test_registry="${transcript_registry}"
kitty-transcript-test-expect 'multiple live sessions in actual tmux session refuse stale registry' 1 ''

# Duplicate live rows for a directly visible foreground pid are equally
# ambiguous; returning whichever row appeared first would be nondeterministic.
kitty-transcript-test-reset
kitty_transcript_test_window_row=$'7\tdirect pid\t777\tcodex'
kitty_transcript_test_live="$(kitty-transcript-test-live-row 777 direct-a A "${transcript_new}" -)"$'\n'\
"$(kitty-transcript-test-live-row 777 direct-b B "${transcript_other}" -)"
kitty-transcript-test-expect 'multiple live rows for direct foreground pid refuse' 1 ''

print -r -- 'ok: kitty transcript resolution'
