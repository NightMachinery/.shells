#!/usr/bin/env zsh
# Isolated fixture-backed orchestration checks.
source "${0:A:h}/fixtures/agent-handoff.zsh"
source "${handoff_test_root}/zshlang/auto-load/others/agent-handoff-reverse.zsh"
typeset -g original_pwd="${PWD}"
typeset -gx CLAUDECODE=1 CODEX_THREAD_ID=stale AGENT_SESSION_REUSE_PANE=stale CLAUDE_CONFIG_DIR=stale
handoff_test_owner=codex
codex-to-claude source --model opus
expect-log 'claude unset'
expect-log "'--model' 'opus' '--'"
typeset -a histories=( "${agent_handoff_state_dir}"/handoff.*/history.md(N) )
(( ${#histories} == 1 ))
zmodload zsh/stat
typeset -A history_stat
zstat -H history_stat "${histories[1]}"
(( (history_stat[mode] & 8#777) == 8#600 ))
[[ "$(<"${histories[1]}")" == 'Historical user request and tool output.' ]]
: > "${handoff_test_log}"
agent_handoff_claude_profile=work codex-to-claude-all-fz --effort high
expect-log 'picker codex all'
expect-log "claude ${handoff_test_tmp}/.work"
if codex-to-claude source --resume wrong ; then exit 1; fi
[[ "${CLAUDE_CONFIG_DIR}" == stale && "${PWD}" == "${original_pwd}" ]]
print -r -- 'agent-handoff orchestration checks passed'
