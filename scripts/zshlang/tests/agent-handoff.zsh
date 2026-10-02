#!/usr/bin/env zsh
# Isolated fixture-backed orchestration checks.
source "${0:A:h}/fixtures/agent-handoff.zsh"
typeset -g original_pwd="${PWD}"
typeset -gx CLAUDECODE=1 CODEX_THREAD_ID=stale AGENT_SESSION_REUSE_PANE=stale CLAUDE_CONFIG_DIR=stale
: > "${handoff_test_log}"
claude-to-codex-compact source --model 'gpt-6.1-sol' --config 'model_reasoning_effort="high"' --config model_context_window=272000 --no-alt-screen
expect-log "'-mode' 'compact'"
expect-log "'--model' 'gpt-6.1-sol'"
expect-log "launch 'resume' '${handoff_test_id}' '--model' 'gpt-6.1-sol'"
[[ "$(<"${handoff_test_log}")" == *"'--no-alt-screen' '--config' 'model_context_window=1050000'"* ]]
[[ "${PWD}" == "${original_pwd}" && "${CODEX_THREAD_ID}" == stale ]]
: > "${handoff_test_log}"
claude-to-codex-native-all-fz --model=gpt-6-astra
expect-log 'picker claude all'
expect-log "'-mode' 'native'"
expect-log "launch 'resume' '${handoff_test_id}' '--model=gpt-6-astra'"
: > "${handoff_test_log}"
handoff_test_pick_cancel=y
if claude-to-codex-native-fz ; then exit 1; fi
[[ "$(<"${handoff_test_log}")" == 'picker claude project' ]]
handoff_test_pick_cancel=n
: > "${handoff_test_log}"
handoff_test_backend_fail=y
if claude-to-codex-compact source ; then exit 1; fi
[[ "$(<"${handoff_test_log}")" != *launch* ]]
handoff_test_backend_fail=n
: > "${handoff_test_log}"
handoff_test_live=$'42\tid\tname\tcwd\t'"${handoff_test_source}"$'\ttmux\tworking\tinteractive'
if claude-to-codex-native source ; then exit 1; fi
[[ ! -s "${handoff_test_log}" ]]
handoff_test_live=''
if claude-to-codex-native source --profile long ; then exit 1; fi
if claude-to-codex-native source --cd /tmp ; then exit 1; fi
if claude-to-codex-native source '$(printf sentinel)' ; then exit 1; fi
[[ ! -s "${handoff_test_log}" ]]
print -r -- 'agent-handoff orchestration checks passed'
