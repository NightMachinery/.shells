#!/usr/bin/env zsh
# Hermetic regression tests for Claude's tmux identity hook.
# Run with: zsh -f zshlang/tests/agent-tmux-claude.zsh

setopt errexit nounset pipefail typesetsilent

typeset -gr claude_tmux_test_scripts="${0:A:h:h:h}"
typeset -g claude_tmux_test_tmp
claude_tmux_test_tmp="$(command mktemp -d "${TMPDIR:-/tmp}/claude-tmux-tests.XXXXXX")"
function claude-tmux-test-cleanup {
    if [[ -n "${claude_tmux_test_tmp:-}" && -d "${claude_tmux_test_tmp}" &&
        "${claude_tmux_test_tmp:t}" == claude-tmux-tests.* ]] ; then
        command rm -rf -- "${claude_tmux_test_tmp}"
    fi
}
trap claude-tmux-test-cleanup EXIT

typeset -gx XDG_STATE_HOME="${claude_tmux_test_tmp}/state"
typeset -gx CLAUDE_TMUX_TEST_CALLS="${claude_tmux_test_tmp}/tmux-calls.jsonl"
typeset -gx CLAUDE_TMUX_TEST_IDENTITY_STATUS=0
typeset -gr claude_tmux_test_id='12345678-1234-1234-1234-123456789abc'
typeset -gr claude_tmux_test_secret='PROMPT-DATA-MUST-NOT-APPEAR'
typeset -gr claude_tmux_test_transcript="${claude_tmux_test_tmp}/transcript.jsonl"
typeset -g claude_tmux_test_payload
command mkdir -p -- "${claude_tmux_test_tmp}/bin"
command cat > "${claude_tmux_test_tmp}/bin/tmux" <<'EOF'
#!/bin/sh
command jq --null-input --compact-output --args '$ARGS.positional' -- "$@" >> "$CLAUDE_TMUX_TEST_CALLS"
case "$1" in
    set-option) exit "$CLAUDE_TMUX_TEST_IDENTITY_STATUS" ;;
    show-option)
        case "$2" in
            -pqv) exit 0 ;;
            -qvA) printf '%s\n' on ;;
            *) exit 99 ;;
        esac ;;
    display-message) printf '%s\n' current ;;
    rename-session) exit 88 ;;
    *) exit 99 ;;
esac
EOF
command chmod +x "${claude_tmux_test_tmp}/bin/tmux"
typeset -gx PATH="${claude_tmux_test_tmp}/bin:${PATH}"

# Load only the helpers needed here, with no personal shell or real server.
source "${claude_tmux_test_scripts}/zshlang/basic/core.zsh"
source "${claude_tmux_test_scripts}/zshlang/basic/return-aliases.zsh"
function aliasfn { :; }
source "${claude_tmux_test_scripts}/zshlang/plugins/agent-session/session.zsh"
source "${claude_tmux_test_scripts}/zshlang/auto-load/others/agent-tmux.zsh"
function h-agent-session-tmux-name { print -r -- '+Claude/default fixture'; }

function claude-tmux-test-fail {
    print -ru2 -- "FAIL: $*"
    exit 1
}

function claude-tmux-test-reset {
    : >| "${CLAUDE_TMUX_TEST_CALLS}"
    CLAUDE_TMUX_TEST_IDENTITY_STATUS=0
    command rm -rf -- "${XDG_STATE_HOME}/agent-sessions"
}

function claude-tmux-test-expect-status {
    local key="${1}" outcome="${2}" id="${3-${claude_tmux_test_id}}"
    local status_file="${XDG_STATE_HOME}/agent-sessions/hooks/claude-${key}.json"
    command jq --exit-status --arg id "${id}" --arg outcome "${outcome}" '
        keys == ["agent", "id", "outcome", "pane", "timestamp"] and
        .agent == "claude" and .id == $id and .outcome == $outcome and
        (.timestamp | type == "string" and length > 0)
    ' "${status_file}" >/dev/null || claude-tmux-test-fail 'unexpected diagnostic'
    [[ "$(<"${status_file}")" != *"${claude_tmux_test_secret}"* &&
        "$(<"${status_file}")" != *"${claude_tmux_test_transcript}"* ]] ||
        claude-tmux-test-fail 'diagnostic leaked prompt or transcript path'
    [[ "$(command ls -ld "${status_file}")" == -rw-------* ]] ||
        claude-tmux-test-fail 'diagnostic is not mode 600'
    local -a leftovers=( "${status_file:h}"/.claude-*(N) )
    (( ${#leftovers} == 0 )) || claude-tmux-test-fail 'temporary diagnostic left behind'
}

function claude-tmux-test-expect-identity {
    local transcript="${1}"
    command jq --exit-status --slurp --arg id "${claude_tmux_test_id}" \
        --arg transcript "${transcript}" '
        [.[] | select(.[0] == "set-option")] ==
        [["set-option", "-t", "%12", "@agent_session", "claude\t" + $id + "\t" + $transcript]]
    ' "${CLAUDE_TMUX_TEST_CALLS}" >/dev/null ||
        claude-tmux-test-fail 'identity write received unexpected arguments'
}

claude_tmux_test_payload="$(command jq --null-input --compact-output \
    --arg id "${claude_tmux_test_id}" --arg transcript "${claude_tmux_test_transcript}" \
    --arg prompt "${claude_tmux_test_secret}" \
    '{session_id:$id,transcript_path:$transcript,prompt:$prompt}')"

# Valid argv payload records the identity even when optional renaming fails.
claude-tmux-test-reset
claude-code-session-tmux-autoname %12 "${claude_tmux_test_payload}"
claude-tmux-test-expect-identity "${claude_tmux_test_transcript}"
claude-tmux-test-expect-status 12 recorded
command jq --exit-status --slurp 'any(.[]; .[0] == "rename-session")' \
    "${CLAUDE_TMUX_TEST_CALLS}" >/dev/null || claude-tmux-test-fail 'rename not exercised'

# The hook's actual stdin interface must behave identically.
claude-tmux-test-reset
claude-code-session-tmux-autoname %12 <<< "${claude_tmux_test_payload}"
claude-tmux-test-expect-identity "${claude_tmux_test_transcript}"
claude-tmux-test-expect-status 12 recorded

# A missing pane must not attempt any tmux command.
claude-tmux-test-reset
claude-code-session-tmux-autoname '' "${claude_tmux_test_payload}"
[[ ! -s "${CLAUDE_TMUX_TEST_CALLS}" ]] || claude-tmux-test-fail 'missing pane touched tmux'
claude-tmux-test-expect-status no-pane no-pane

# Reproduce a stale-pane write failure through the production identity writer.
claude-tmux-test-reset
CLAUDE_TMUX_TEST_IDENTITY_STATUS=1
claude-code-session-tmux-autoname %12 "${claude_tmux_test_payload}"
claude-tmux-test-expect-identity "${claude_tmux_test_transcript}"
claude-tmux-test-expect-status 12 identity-failed
command jq --exit-status --slurp 'all(.[]; .[0] != "rename-session")' \
    "${CLAUDE_TMUX_TEST_CALLS}" >/dev/null || claude-tmux-test-fail 'renamed after failed write'
typeset core_status=0
h-agent-tmux-autoname claude %12 "${claude_tmux_test_id}" "${claude_tmux_test_transcript}" || core_status=$?
(( core_status == 1 )) || claude-tmux-test-fail 'shared core stopped propagating errors'

# An ID without a transcript is still useful and must not claim a full record.
claude-tmux-test-reset
claude-code-session-tmux-autoname %12 "{\"session_id\":\"${claude_tmux_test_id}\"}"
claude-tmux-test-expect-identity ''
claude-tmux-test-expect-status 12 recorded-id

# Invalid payloads cannot overwrite identity or leak arbitrary input into status.
typeset invalid_payload
for invalid_payload in '{' '{}' '{"session_id":42}' ; do
    claude-tmux-test-reset
    claude-code-session-tmux-autoname %12 "${invalid_payload}"
    [[ ! -s "${CLAUDE_TMUX_TEST_CALLS}" ]] || claude-tmux-test-fail 'invalid input touched tmux'
    claude-tmux-test-expect-status 12 invalid-payload ''
done

# A broken diagnostic destination is also optional, including after a failure.
claude-tmux-test-reset
: >| "${claude_tmux_test_tmp}/not-a-directory"
XDG_STATE_HOME="${claude_tmux_test_tmp}/not-a-directory" \
    CLAUDE_TMUX_TEST_IDENTITY_STATUS=1 \
    claude-code-session-tmux-autoname %12 "${claude_tmux_test_payload}"

print -r -- 'ok: Claude tmux autoname hook'
