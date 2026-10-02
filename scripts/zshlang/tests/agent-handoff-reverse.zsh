#!/usr/bin/env zsh
# Isolated fixture-backed orchestration checks.
source "${0:A:h}/fixtures/agent-handoff.zsh"
source "${handoff_test_root}/zshlang/plugins/agent-session/session.zsh"
source "${handoff_test_root}/zshlang/auto-load/others/claude-session.zsh"
source "${handoff_test_root}/zshlang/auto-load/others/agent-handoff-reverse.zsh"
function h-claude-code-profile-assert { [[ "${1}" == (default|work) ]]; }
function h-claude-code-profile-config-home {
    [[ "${1}" == default ]] && print -r -- "${HOME}/.claude" || print -r -- "${handoff_test_tmp}/.work"
}
functions[handoff-original-backend]="${functions[agent_session]}"
functions[handoff-original-claude]="${functions[test-claude]}"
typeset -g handoff_test_seed_fail=n handoff_test_compact_fail=n handoff_test_verify_fail=n
function h-claude-code-session-live-list { print -r -- "${handoff_test_live}"; }
function agent_session {
    if [[ "${2}" == handoff-claude ]] ; then
        print -r -- "seed ${(j: :)${(qq)@}}" >> "${handoff_test_log}"
        [[ "${handoff_test_seed_fail}" == n ]] || return 19
        local dest="${handoff_test_tmp}/seed/projects/p/${handoff_test_id}.jsonl"
        command mkdir -p -- "${dest:h}"
        print '{}' > "${dest}"
        print -r -- "${dest}"
    elif [[ "${2}" == compact-result ]] ; then
        print -r -- "verify ${4}" >> "${handoff_test_log}"
        [[ "${handoff_test_verify_fail}" == n ]]
    else
        handoff-original-backend "$@"
    fi
}
function test-claude {
    if (( ${@[(Ie)--print]} )) ; then
        [[ "${PWD}" == "${handoff_test_dir}" && -z "${CODEX_THREAD_ID:-}" ]]
        print -r -- "prepare ${CLAUDE_CONFIG_DIR:-unset} ${(j: :)${(qq)@}}" >> "${handoff_test_log}"
        [[ "${handoff_test_compact_fail}" == n ]] || return 7
        print '{"type":"result"}'
    else
        handoff-original-claude "$@"
    fi
}
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
: > "${handoff_test_log}"
codex-to-claude-compact-fz --model opus
expect-log 'picker codex project'
expect-log "'-config-home' '${HOME}/.claude'"
expect-log "prepare unset '--resume' '${handoff_test_id}' '--model' 'opus'"
expect-log "'--tools' '' '--' '/compact'"
expect-log "verify ${handoff_test_id}"
expect-log "claude unset '--resume' '${handoff_test_id}' '--model' 'opus' '--'"
[[ "$(<"${handoff_test_log}")" == *"verify ${handoff_test_id}"*"claude unset"* ]]
: > "${handoff_test_log}"
agent_handoff_claude_profile=work codex-to-claude-compact-all-fz --effort high
expect-log 'picker codex all'
expect-log "prepare ${handoff_test_tmp}/.work"
expect-log "claude ${handoff_test_tmp}/.work '--resume' '${handoff_test_id}'"
for kind in seed compact verify ; do
    : > "${handoff_test_log}"
    typeset -g "handoff_test_${kind}_fail=y"
    if codex-to-claude-compact source ; then exit 1; fi
    [[ "$(<"${handoff_test_log}")" != *'claude unset'* ]]
    typeset -g "handoff_test_${kind}_fail=n"
done
: > "${handoff_test_log}"
handoff_test_pick_cancel=y
if codex-to-claude-compact-all-fz ; then exit 1; fi
[[ "$(<"${handoff_test_log}")" == 'picker codex all' ]]
handoff_test_pick_cancel=n
: > "${handoff_test_log}"
handoff_test_live=$'42\tid\tname\tcwd\t'"${handoff_test_source}"$'\ttmux\tworking\tinteractive'
if codex-to-claude-compact source ; then exit 1; fi
[[ ! -s "${handoff_test_log}" ]]
[[ "${CLAUDE_CONFIG_DIR}" == stale && "${PWD}" == "${original_pwd}" ]]
print -r -- 'agent-handoff orchestration checks passed'
