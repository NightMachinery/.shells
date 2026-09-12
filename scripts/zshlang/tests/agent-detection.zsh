#!/usr/bin/env zsh
# Regression tests for agent environment detection. Run with: zsh -f this-file

setopt errexit nounset pipefail

typeset -gr agent_detection_test_root="${0:A:h:h:h}"
source "${agent_detection_test_root}/zshlang/basic/conditions.zsh"

function ec {
    print -r -- "$@"
}

function agent-detection-test-fail {
    print -ru2 -- "FAIL: $*"
    return 1
}

function agent-detection-test-case {
    local description="${1}"
    local expected_name="${2}"
    shift 2

    (
        typeset -gx AI_AGENT='' CLAUDECODE='' CODEX_SANDBOX=''
        typeset -gx CODEX_THREAD_ID='' CODEX_SESSION_ID=''
        typeset -gx ANTIGRAVITY_AGENT='' ANTIGRAVITY_TRAJECTORY_ID=''
        typeset -gx ANTIGRAVITY_CONVERSATION_ID=''
        if (( $# )) ; then
            typeset -gx "$@"
        fi

        if [[ -n "${expected_name}" ]] ; then
            codex-p || agent-detection-test-fail "${description}: codex-p should succeed"
            ai-agent-p || agent-detection-test-fail "${description}: ai-agent-p should succeed"
            [[ "$(ai-agent-name)" == "${expected_name}" ]] || \
                agent-detection-test-fail "${description}: ai-agent-name should print ${expected_name}"
        else
            ! codex-p || agent-detection-test-fail "${description}: codex-p should fail"
            ! ai-agent-p || agent-detection-test-fail "${description}: ai-agent-p should fail"
            ! ai-agent-name >/dev/null || \
                agent-detection-test-fail "${description}: ai-agent-name should fail"
        fi
    )
}

agent-detection-test-case 'thread ID only, unsandboxed' codex CODEX_THREAD_ID=thread-123
agent-detection-test-case 'session ID only, unsandboxed' codex CODEX_SESSION_ID=session-123
agent-detection-test-case 'sandbox marker' codex CODEX_SANDBOX=seatbelt
agent-detection-test-case 'legacy AI_AGENT marker' codex AI_AGENT=codex_agent
agent-detection-test-case 'no agent markers' ''

print -r -- 'ok: agent environment detection'
