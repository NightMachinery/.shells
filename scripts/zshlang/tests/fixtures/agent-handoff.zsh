#!/usr/bin/env zsh
# Isolated orchestration checks. No installed agent or live session is used.
setopt errexit pipefail typesetsilent
typeset -gr handoff_test_root="${${(%):-%x}:A:h:h:h:h}"
typeset -g handoff_test_tmp="$(command mktemp -d "${TMPDIR:-/tmp}/agent-handoff-tests.XXXXXX")"
trap 'command rm -rf -- "${handoff_test_tmp}"' EXIT
unset night_basic_plugin_loaded_p
source "${handoff_test_root}/zshlang/basic/basic.plugin.zsh"
unalias aliasfn 2>/dev/null || true
setopt nounset
function aliasfn {
    local name="${1}"
    shift
    functions[${name}]="${(j: :)${(q)@}} \"\$@\""
}
source "${handoff_test_root}/zshlang/auto-load/others/agent-handoff.zsh"
unalias ecerr 2>/dev/null || true
function ecerr { print -ru2 -- "$@"; }
typeset -g handoff_test_source="${handoff_test_tmp}/source.jsonl"
typeset -g handoff_test_dir="${handoff_test_tmp}/project with spaces"
typeset -g handoff_test_log="${handoff_test_tmp}/calls"
typeset -g handoff_test_live='' handoff_test_owner=claude handoff_test_backend_fail=n
typeset -g handoff_test_pick_cancel=n
typeset -g handoff_test_id=11111111-1111-4111-8111-111111111111
typeset -gA claude_code_profile_launchers=(default test-claude work test-claude)
typeset -g agent_handoff_state_dir="${handoff_test_tmp}/state"
command mkdir -- "${handoff_test_dir}"
print -r -- '{}' > "${handoff_test_source}"
function h-agent-session-dep { :; }
function ensure-cmd { :; }
function h-agent-session-resolve { print -r -- "${handoff_test_source}"; }
function h-agent-session-agent-of { print -r -- "${handoff_test_owner}"; }
function h-agent-session-live-list { print -r -- "${handoff_test_live}"; }
function h-agent-session-dir { print -r -- "${handoff_test_dir}"; }
function h-tmux-env-repair { :; }
function h-claude-code-profile-assert { [[ "${1}" == (default|work) ]]; }
function h-claude-code-profile-config-home { print -r -- "${handoff_test_tmp}/.${1}"; }
function gmktemp { command mktemp "$@"; }
function agent_session {
    print -r -- "backend ${(j: :)${(qq)@}}" >> "${handoff_test_log}"
    [[ "${handoff_test_backend_fail}" == n ]] || return 17
    if [[ "${2}" == handoff-export ]] ; then
        print -r -- 'Historical user request and tool output.'
    else
        [[ -z "${CODEX_THREAD_ID:-}" && -z "${CLAUDECODE:-}" && -z "${AGENT_SESSION_REUSE_PANE:-}" ]]
        print -r -- "${handoff_test_id}"
    fi
}
function codex-m {
    [[ "${PWD}" == "${handoff_test_dir}" ]]
    [[ -z "${CLAUDECODE:-}" && -z "${AGENT_SESSION_REUSE_PANE:-}" ]]
    print -r -- "launch ${(j: :)${(qq)@}}" >> "${handoff_test_log}"
}
function test-claude {
    [[ "${PWD}" == "${handoff_test_dir}" ]]
    [[ -z "${CODEX_THREAD_ID:-}" && -z "${AGENT_SESSION_REUSE_PANE:-}" ]]
    print -r -- "claude ${CLAUDE_CONFIG_DIR:-unset} ${(j: :)${(qq)@}}" >> "${handoff_test_log}"
}
function h-agent-session-select-fz {
    print -r -- "picker ${agent_session_agents} ${agent_session_fz_scope}" >> "${handoff_test_log}"
    [[ "${handoff_test_pick_cancel}" == n ]] || return 130
    print -r -- "${handoff_test_source}"
}
function expect-log {
    local contents="$(<"${handoff_test_log}")"
    [[ "${contents}" == *"${1}"* ]] || { print -ru2 -- "Missing log: ${1}"; return 1; }
}
