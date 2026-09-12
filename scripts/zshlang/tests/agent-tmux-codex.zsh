#!/usr/bin/env zsh
# Hermetic regression tests for the Codex tmux autoname hook.
# Run with: zsh -f zshlang/tests/agent-tmux-codex.zsh

setopt errexit nounset pipefail typesetsilent

typeset -gr codex_tmux_test_scripts="${0:A:h:h:h}"
typeset -g codex_tmux_test_tmp
codex_tmux_test_tmp="$(command mktemp -d "${TMPDIR:-/tmp}/codex-tmux-tests.XXXXXX")"

function codex-tmux-test-cleanup {
    if [[ -n "${codex_tmux_test_tmp:-}" && -d "${codex_tmux_test_tmp}" &&
        "${codex_tmux_test_tmp:t}" == codex-tmux-tests.* ]] ; then
        command rm -rf -- "${codex_tmux_test_tmp}"
    fi
}
trap codex-tmux-test-cleanup EXIT

typeset -gx XDG_STATE_HOME="${codex_tmux_test_tmp}/state"
typeset -g codex_tmux_test_capture="${codex_tmux_test_tmp}/autoname.jsonl"
typeset -g codex_tmux_test_resolve_calls="${codex_tmux_test_tmp}/resolve.jsonl"
typeset -g codex_tmux_test_fixture="${codex_tmux_test_tmp}/rollout-fixture.jsonl"
typeset -g codex_tmux_test_tmux_calls="${codex_tmux_test_tmp}/tmux-calls"
typeset -g codex_tmux_test_bin="${codex_tmux_test_tmp}/bin"
typeset -g codex_tmux_test_id='12345678-1234-1234-1234-123456789abc'
typeset -g codex_tmux_test_autoname_status=0
typeset -g codex_tmux_test_resolve_status=0
typeset -g codex_tmux_test_expected_resolve_id=''

command mkdir -p -- "${codex_tmux_test_bin}"
: >| "${codex_tmux_test_capture}"
: >| "${codex_tmux_test_resolve_calls}"
: >| "${codex_tmux_test_fixture}"
: >| "${codex_tmux_test_tmux_calls}"

# Load the public-safe core and production definitions without the personal
# shell. These files use aliasfn only for top-level convenience aliases, none
# of which the tests need.
unset night_basic_plugin_loaded_p
source "${codex_tmux_test_scripts}/zshlang/basic/basic.plugin.zsh"
function aliasfn { :; }
source "${codex_tmux_test_scripts}/zshlang/auto-load/others/agent-session.zsh"
source "${codex_tmux_test_scripts}/zshlang/auto-load/others/codex-session.zsh"
source "${codex_tmux_test_scripts}/zshlang/auto-load/others/agent-tmux.zsh"

# Keep a callable copy before replacing the hook boundary with a fixture.
functions[h-agent-tmux-autoname-production]="${functions[h-agent-tmux-autoname]}"

function codex-tmux-test-fail {
    print -ru2 -- "FAIL: $*"
    return 1
}

function codex-tmux-test-reset {
    : >| "${codex_tmux_test_capture}"
    : >| "${codex_tmux_test_resolve_calls}"
    codex_tmux_test_autoname_status=0
    codex_tmux_test_resolve_status=0
    codex_tmux_test_expected_resolve_id=''
    command rm -rf -- "${XDG_STATE_HOME}/agent-sessions"
}

function codex-tmux-test-status-file {
    local pane="${1}"
    local key=no-pane
    [[ "${pane}" == %<-> ]] && key="${pane#%}"
    print -r -- "${XDG_STATE_HOME}/agent-sessions/hooks/codex-${key}.json"
}

function codex-tmux-test-expect-status {
    local pane="${1}" id="${2}" outcome="${3}" secret="${4:-}"
    local status_file
    status_file="$(codex-tmux-test-status-file "${pane}")"
    [[ -f "${status_file}" ]] ||
        codex-tmux-test-fail "missing diagnostic ${status_file}"
    command jq --exit-status \
        --arg pane "${pane}" --arg id "${id}" --arg outcome "${outcome}" '
        keys == ["agent", "id", "outcome", "pane", "timestamp"] and
        .agent == "codex" and .pane == $pane and .id == $id and
        .outcome == $outcome and
        (.timestamp | type == "string" and length > 0)
    ' "${status_file}" >/dev/null ||
        codex-tmux-test-fail "unexpected diagnostic in ${status_file}"
    if [[ -n "${secret}" ]] ; then
        [[ "$(<"${status_file}")" != *"${secret}"* ]] ||
            codex-tmux-test-fail 'diagnostic leaked prompt or payload data'
    fi
    local -a leftovers
    leftovers=( "${status_file:h}"/.codex-*(N) )
    (( ${#leftovers} == 0 )) ||
        codex-tmux-test-fail 'atomic status writer left a temporary file behind'
}

function codex-tmux-test-expect-autoname {
    local expected_transcript="${1}"
    local count
    count="$(command wc -l < "${codex_tmux_test_capture}")"
    (( count == 1 )) || codex-tmux-test-fail "expected one autoname call, got ${count}"
    command jq --exit-status \
        --arg id "${codex_tmux_test_id}" --arg transcript "${expected_transcript}" '
        .agent == "codex" and .pane == "%12" and .id == $id and
        .transcript == $transcript
    ' "${codex_tmux_test_capture}" >/dev/null ||
        codex-tmux-test-fail 'autoname received unexpected arguments'
}

function codex-tmux-test-expect-no-identity-write {
    [[ ! -s "${codex_tmux_test_capture}" ]] ||
        codex-tmux-test-fail 'hook attempted an identity/autoname write'
}

# Exercise h-agent-tmux-autoname itself without touching a real tmux server.
function h-agent-tmux-identity-set { return 41; }
typeset core_status=0
h-agent-tmux-autoname-production codex %12 "${codex_tmux_test_id}" '' || core_status=$?
(( core_status == 41 )) ||
    codex-tmux-test-fail "identity-set failure was not propagated (got ${core_status})"

print -r -- '#!/bin/sh' >| "${codex_tmux_test_bin}/tmux"
print -r -- 'printf "%s\n" "$*" >> "$CODEX_TMUX_TEST_TMUX_CALLS"' >> "${codex_tmux_test_bin}/tmux"
print -r -- 'case "$1" in' >> "${codex_tmux_test_bin}/tmux"
print -r -- '  display-message) printf "%s\n" current ;;' >> "${codex_tmux_test_bin}/tmux"
print -r -- '  show-option) printf "%s\n" on ;;' >> "${codex_tmux_test_bin}/tmux"
print -r -- '  rename-session) exit 88 ;;' >> "${codex_tmux_test_bin}/tmux"
print -r -- '  *) exit 1 ;;' >> "${codex_tmux_test_bin}/tmux"
print -r -- 'esac' >> "${codex_tmux_test_bin}/tmux"
command chmod +x "${codex_tmux_test_bin}/tmux"
typeset -gx CODEX_TMUX_TEST_TMUX_CALLS="${codex_tmux_test_tmux_calls}"
typeset -gx PATH="${codex_tmux_test_bin}:${PATH}"
function h-agent-tmux-identity-set { return 0; }
function h-agent-session-tmux-name { print -r -- ''; }
core_status=0
h-agent-tmux-autoname-production codex %12 "${codex_tmux_test_id}" '' || core_status=$?
(( core_status == 0 )) ||
    codex-tmux-test-fail "ordinary missing-name path returned ${core_status}"
[[ "$(<"${codex_tmux_test_tmux_calls}")" != *rename-session* ]] ||
    codex-tmux-test-fail 'missing name should not attempt rename-session'

# Hook-level fixtures: capture the identity/autoname boundary and require the
# resolver fallback to receive the Codex provider plus the exact session ID.
function h-agent-tmux-autoname {
    command jq --null-input --compact-output \
        --arg agent "${1}" --arg pane "${2}" --arg id "${3}" --arg transcript "${4}" \
        '{agent:$agent,pane:$pane,id:$id,transcript:$transcript}' \
        >> "${codex_tmux_test_capture}"
    return "${codex_tmux_test_autoname_status}"
}

function h-agent-session-resolve {
    command jq --null-input --compact-output --arg agent "${1}" --arg id "${2}" \
        '{agent:$agent,id:$id}' >> "${codex_tmux_test_resolve_calls}"
    [[ "${1}" == codex && "${2}" == "${codex_tmux_test_expected_resolve_id}" ]] ||
        return 97
    (( codex_tmux_test_resolve_status == 0 )) || return "${codex_tmux_test_resolve_status}"
    print -r -- "${codex_tmux_test_fixture}"
}

typeset payload secret='PROMPT-DATA-MUST-NOT-APPEAR'

# A supplied, existing transcript goes straight to the identity boundary.
codex-tmux-test-reset
payload="$(command jq -nc --arg id "${codex_tmux_test_id}" \
    --arg path "${codex_tmux_test_fixture}" --arg prompt "${secret}" \
    '{session_id:$id,transcript_path:$path,prompt:$prompt}')"
codex-session-tmux-autoname %12 "${payload}"
codex-tmux-test-expect-autoname "${codex_tmux_test_fixture}"
[[ ! -s "${codex_tmux_test_resolve_calls}" ]] ||
    codex-tmux-test-fail 'existing transcript should not invoke resolver fallback'
codex-tmux-test-expect-status %12 "${codex_tmux_test_id}" recorded "${secret}"

# A null transcript path is resolved by provider and exact session ID.
codex-tmux-test-reset
codex_tmux_test_expected_resolve_id="${codex_tmux_test_id}"
payload="$(command jq -nc --arg id "${codex_tmux_test_id}" \
    '{session_id:$id,transcript_path:null}')"
codex-session-tmux-autoname %12 "${payload}"
codex-tmux-test-expect-autoname "${codex_tmux_test_fixture}"
command jq --exit-status --arg id "${codex_tmux_test_id}" \
    '.agent == "codex" and .id == $id' "${codex_tmux_test_resolve_calls}" >/dev/null ||
    codex-tmux-test-fail 'resolver fallback did not receive codex plus the exact ID'
codex-tmux-test-expect-status %12 "${codex_tmux_test_id}" recorded

# If resolution finds nothing, the ID is still recorded with an empty path.
codex-tmux-test-reset
codex_tmux_test_expected_resolve_id="${codex_tmux_test_id}"
codex_tmux_test_resolve_status=1
payload="$(command jq -nc --arg id "${codex_tmux_test_id}" '{session_id:$id}')"
codex-session-tmux-autoname %12 "${payload}"
codex-tmux-test-expect-autoname ''
codex-tmux-test-expect-status %12 "${codex_tmux_test_id}" recorded-id

# Subagents inherit the root ID and must cause neither identity nor diagnostic
# writes. The transcript resolver must not run either.
codex-tmux-test-reset
payload="$(command jq -nc --arg id "${codex_tmux_test_id}" \
    '{session_id:$id,transcript_path:null,agent_id:"subagent-7"}')"
codex-session-tmux-autoname %12 "${payload}"
codex-tmux-test-expect-no-identity-write
[[ ! -s "${codex_tmux_test_resolve_calls}" ]] ||
    codex-tmux-test-fail 'subagent payload invoked resolver fallback'
[[ ! -e "$(codex-tmux-test-status-file %12)" ]] ||
    codex-tmux-test-fail 'subagent payload unexpectedly wrote a diagnostic'

# Invalid JSON and every non-string, missing, or empty ID are rejected before
# identity or transcript resolution. Diagnostics remain safe to write.
typeset invalid_payload
for invalid_payload in \
    '{' \
    '{}' \
    '{"session_id":null}' \
    '{"session_id":""}' \
    '{"session_id":42}'
do
    codex-tmux-test-reset
    codex-session-tmux-autoname %12 "${invalid_payload}"
    codex-tmux-test-expect-no-identity-write
    [[ ! -s "${codex_tmux_test_resolve_calls}" ]] ||
        codex-tmux-test-fail 'invalid payload invoked resolver fallback'
    codex-tmux-test-expect-status %12 '' invalid-payload
done

# No pane is an ordinary success and must return before any mutation.
codex-tmux-test-reset
payload="$(command jq -nc --arg id "${codex_tmux_test_id}" '{session_id:$id}')"
codex-session-tmux-autoname '' "${payload}"
codex-tmux-test-expect-no-identity-write
[[ ! -s "${codex_tmux_test_resolve_calls}" ]] ||
    codex-tmux-test-fail 'missing pane invoked resolver fallback'
codex-tmux-test-expect-status '' "${codex_tmux_test_id}" no-pane

# The parent hook observes an identity/autoname failure, records it, and still
# exits successfully so a diagnostic problem cannot break Codex itself.
codex-tmux-test-reset
codex_tmux_test_autoname_status=29
payload="$(command jq -nc --arg id "${codex_tmux_test_id}" \
    --arg path "${codex_tmux_test_fixture}" --arg prompt "${secret}" \
    '{session_id:$id,transcript_path:$path,prompt:$prompt}')"
codex-session-tmux-autoname %12 "${payload}"
codex-tmux-test-expect-autoname "${codex_tmux_test_fixture}"
codex-tmux-test-expect-status %12 "${codex_tmux_test_id}" identity-failed "${secret}"

print -r -- 'ok: Codex tmux autoname hook'
