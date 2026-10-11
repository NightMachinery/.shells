#!/usr/bin/env zsh
# Paseo agents in the session pickers: rows from stored records, the duplicate
# filter, and picker pairs. Fixture-only: no daemon, no live session.
# Run with: zsh -f zshlang/tests/paseo-agents.zsh

setopt errexit pipefail typesetsilent

typeset -gr paseo_agents_test_root="${0:A:h:h:h}"
typeset -g paseo_agents_test_tmp
paseo_agents_test_tmp="$(command mktemp -d "${TMPDIR:-/tmp}/paseo-agents-tests.XXXXXX")"

function paseo-agents-test-cleanup {
    if [[ -n "${paseo_agents_test_tmp:-}" && -d "${paseo_agents_test_tmp}" &&
        "${paseo_agents_test_tmp:t}" == paseo-agents-tests.* ]] ; then
        command rm -rf -- "${paseo_agents_test_tmp}"
    fi
}
trap paseo-agents-test-cleanup EXIT

unset night_basic_plugin_loaded_p
source "${paseo_agents_test_root}/zshlang/basic/basic.plugin.zsh"
function aliasfn { :; }
function aliasfnq { :; }
source "${paseo_agents_test_root}/zshlang/auto-load/others/paseo.zsh"
source "${paseo_agents_test_root}/zshlang/auto-load/others/agent-usage.zsh"
source "${paseo_agents_test_root}/zshlang/auto-load/others/agent-auto-continue.zsh"
source "${paseo_agents_test_root}/zshlang/auto-load/others/codex-session.zsh"
source "${paseo_agents_test_root}/zshlang/auto-load/others/codex.zsh"
unalias ecerr 2>/dev/null || true
function ecerr { print -ru2 -- "$@"; }
setopt nounset

typeset -g t="${paseo_agents_test_tmp}"
typeset -g live_id=22222222-2222-4222-8222-222222222222
typeset -g idle_id=33333333-3333-4333-8333-333333333333
typeset -g a_live=aaaaaaaa-0000-4000-8000-000000000001
typeset -g a_idle=aaaaaaaa-0000-4000-8000-000000000002
typeset -g a_arch=aaaaaaaa-0000-4000-8000-000000000003
typeset -g native_t="${t}/claude/projects/-p/${live_id}.jsonl"
typeset -g idle_t="${t}/codex/sessions/2026/10/11/rollout-2026-10-11T00-00-00-${idle_id}.jsonl"
typeset -g other_t="${t}/claude/projects/-p/other.jsonl"
command mkdir -p -- "${t}/home/agents/-p" "${native_t:h}" "${idle_t:h}"
: > "${native_t}" ; : > "${idle_t}" ; : > "${other_t}"

function paseo-agents-test-record {
    #: Usage: paseo-agents-test-record <id> <provider> <native> <status> <title> [archived]
    local archived=null
    [[ "${6:-}" == archived ]] && archived='"2026-10-11T00:00:00Z"'
    print -r -- "{\"id\":\"${1}\",\"provider\":\"p-${2}\",\"title\":\"${5}\",\"lastStatus\":\"${4}\",\"archivedAt\":${archived},\"internal\":false,\"persistence\":{\"nativeHandle\":\"${3}\",\"metadata\":{\"provider\":\"${2}\"}}}" \
        > "${t}/home/agents/-p/${1}.json"
}
paseo-agents-test-record "${a_live}" claude "${live_id}" running 'Live one'
paseo-agents-test-record "${a_idle}" codex "${idle_id}" idle 'Idle\tone'
paseo-agents-test-record "${a_arch}" claude 44444444-4444-4444-8444-444444444444 closed 'Gone' archived

typeset -g paseo_home="${t}/home"
unset PASEO_HOST PASEO_HOME 2>/dev/null || true

#: pid, id, name, cwd, transcript, tmux, status, kind
function h-agent-session-live-list {
    printf '1\t%s\tnative\t/p\t%s\t-\tbusy\tinteractive\n' "${live_id}" "${native_t}"
    printf '2\tother\tother\t/p\t%s\t-\tidle\tinteractive\n' "${other_t}"
}
function h-agent-session-call {
    [[ "${2}" == roots ]] || return 1
    case "${1}" in
        claude) print -r -- "${t}/claude/projects" ;;
        codex) print -r -- "${t}/codex/sessions" ;;
    esac
}

function expect-eq {
    if [[ "${1}" != "${2}" ]] ; then
        print -ru2 -- "FAIL: ${3}"
        print -ru2 -- "  want: ${(q+)2}"
        print -ru2 -- "  got:  ${(q+)1}"
        exit 1
    fi
}

# Rows: archived agents are left out; tabs in a title cannot break columns.
rows="$(paseo-ls | command sort)"
expect-eq "${rows}" "${a_live}"$'\t'claude$'\t'"${live_id}"$'\t'running$'\t'p-claude$'\t''Live one'$'\t-\n'"${a_idle}"$'\t'codex$'\t'"${idle_id}"$'\t'idle$'\t'p-codex$'\t''Idle one'$'\t-' \
    'rows from stored records'
expect-eq "$(paseo-ls --json | command jq --raw-output 'map(.nativeSessionId) | sort | join(" ")')" "${live_id} ${idle_id}" \
    'paseo-ls --json'

# The filter drops exactly the native row a Paseo agent runs.
kept="$(printf 'w1\t%s\tclaude\tnative\nw2\t%s\tclaude\tother\n' "${native_t}" "${other_t}" | h-paseo-filter-pairs)"
expect-eq "${kept}" "w2"$'\t'"${other_t}"$'\t'claude$'\t'other 'filter drops the native row'

# Pairs: the live agent's transcript from the listing, the idle one's by glob.
pairs="$(h-paseo-agent-pairs paseo: | command sort)"
expect-eq "${pairs}" "paseo:${a_live}"$'\t'"${native_t}"$'\t'claude$'\t'"paseo:${a_live[1,8]}  Live one (running)"$'\t'"${paseo_agent_badge}"$'\t0\t?\n'"paseo:${a_idle}"$'\t'"${idle_t}"$'\t'codex$'\t'"paseo:${a_idle[1,8]}  Idle one (idle)"$'\t'"${paseo_agent_badge}"$'\t0\t?' \
    'picker pairs'

# Parent labels identify workflow children, while the complete listing still
# hides their native sessions. Missing transcripts have a stored timestamp.
typeset -g a_child=aaaaaaaa-0000-4000-8000-000000000004
typeset -g child_id=55555555-5555-4555-8555-555555555555
typeset -g child_t="${t}/claude/projects/-p/${child_id}.jsonl"
: > "${child_t}"
paseo-agents-test-record "${a_child}" claude "${child_id}" idle 'Workflow child'
command jq --arg parent "${a_live}" '.labels={"paseo.parent-agent-id":$parent}' \
    "${t}/home/agents/-p/${a_child}.json" > "${t}/child.json"
command mv -- "${t}/child.json" "${t}/home/agents/-p/${a_child}.json"
expect-eq "$(paseo-ls --json | command jq --raw-output 'length')" 3 'listing retains children'
expect-eq "$(h-paseo-agent-pairs paseo: | command cut -f1 | command sort)" \
    "paseo:${a_live}"$'\n'"paseo:${a_idle}" 'picker excludes children'
expect-eq "$(agent_session_fz_subagents_p=y h-paseo-agent-pairs paseo: | command cut -f1 | command sort)" \
    "paseo:${a_live}"$'\n'"paseo:${a_idle}"$'\n'"paseo:${a_child}" 'picker child opt-in'
function h-agent-session-live-list {
    printf '1\t%s\tnative\t/p\t%s\t-\tbusy\tinteractive\n' "${live_id}" "${native_t}"
    printf '2\tother\tother\t/p\t%s\t-\tidle\tinteractive\n' "${other_t}"
    printf '3\t%s\tchild\t/p\t%s\t-\tidle\tinteractive\n' "${child_id}" "${child_t}"
}
kept="$(printf 'w1\t%s\tclaude\tchild\nw2\t%s\tclaude\tother\n' "${child_t}" "${other_t}" | h-paseo-filter-pairs)"
expect-eq "${kept}" "w2"$'\t'"${other_t}"$'\t'claude$'\t'other 'child native stays hidden'
command jq '.lastActivityAt="2026-10-11T12:00:00.123Z" | .lastUserMessageAt="2026-10-11T11:00:00Z"' \
    "${t}/home/agents/-p/${a_idle}.json" > "${t}/stamp.json"
command mv -- "${t}/stamp.json" "${t}/home/agents/-p/${a_idle}.json"
expect-eq "$(h-paseo-agent-pairs paseo: | command tail -n1 | command cut -f6,7)" \
    $'1791720000\t2026-10-11T12:00:00.123Z' 'stored activity fallback'
expect-eq "$(agent_session_rows_sort=user h-paseo-agent-pairs paseo: | command tail -n1 | command cut -f6,7)" \
    $'1791716400\t2026-10-11T11:00:00Z' 'stored user timestamp'
command rm -- "${t}/home/agents/-p/${a_child}.json"

# A remote daemon is refused, and the filter then passes everything through.
if PASEO_HOST=remote:1 paseo-ls 2>/dev/null ; then
    print -ru2 -- 'FAIL: PASEO_HOST was not refused' ; exit 1
fi
kept="$(printf 'w1\t%s\tclaude\tnative\n' "${native_t}" | PASEO_HOST=remote:1 h-paseo-filter-pairs 2>/dev/null)"
expect-eq "${kept}" "w1"$'\t'"${native_t}"$'\t'claude$'\t'native 'filter is a no-op without Paseo'

# Picked rows: a Paseo row already names its target.
targets="$(printf 'paseo:%s\t%s\n' "${a_live}" "${native_t}" | h-agent-usage-continue-rows-to-targets tmux)"
expect-eq "${targets}" "paseo:${a_live}" 'a Paseo row maps to its own target'

# Sending: a stub CLI logs its argv and answers `inspect' from a fixture.
command mkdir -p -- "${t}/bin"
cat > "${t}/bin/paseo" <<'SH'
#!/bin/sh
printf '%s\n' "$*" >> "${PASEO_TEST_LOG}"
[ "$1" = inspect ] && printf '%s\n' "${PASEO_TEST_INSPECT}"
[ "$1" = ls ] && printf '%s\n' "${PASEO_TEST_LS}"
exit 0
SH
command chmod 700 "${t}/bin/paseo"
path=("${t}/bin" "${path[@]}")
typeset -gx PASEO_TEST_LOG="${t}/paseo.log" PASEO_TEST_INSPECT='' PASEO_TEST_LS=''

function paseo-agents-test-send {
    #: Usage: paseo-agents-test-send <inspect-json> <target>; prints the log.
    : > "${PASEO_TEST_LOG}"
    PASEO_TEST_INSPECT="${1}"
    h-agent-usage-continue-send "${2}" >/dev/null 2>&1 || print -r -- refused
    command cat -- "${PASEO_TEST_LOG}"
}

out="$(paseo-agents-test-send '{"Archived":false,"Status":"idle","PendingPermissions":[]}' "paseo:${a_idle}")"
expect-eq "${out}" "inspect ${a_idle} --home ${paseo_home} --json"$'\n'"send ${a_idle} --prompt Continue. --no-wait --json --home ${paseo_home}" \
    'an idle agent is sent to'

for case in '{"Archived":true,"Status":"idle"}' '{"Archived":false,"Status":"running"}' \
        '{"Archived":false,"Status":"idle","PendingPermissions":[{"id":1}]}' ; do
    out="$(paseo-agents-test-send "${case}" "paseo:${a_idle}")"
    expect-eq "${out}" "refused"$'\n'"inspect ${a_idle} --home ${paseo_home} --json" "no send when ${case}"
done

out="$(paseo-agents-test-send '{}' 'paseo:not-a-uuid')"
expect-eq "${out}" refused 'a malformed id is refused before any call'

# paseo-ls-check: the same ids pass even with a status that moved on; a
# missing agent fails.
PASEO_TEST_LS="[{\"id\":\"${a_live}\",\"status\":\"idle\"},{\"id\":\"${a_idle}\",\"status\":\"idle\"}]"
out="$(paseo-ls-check)"
expect-eq "${out}" "note: status running here, idle per the daemon: ${a_live}"$'\n''0 mismatched of 2 agents' 'paseo-ls-check agreement'
PASEO_TEST_LS="[{\"id\":\"${a_live}\",\"status\":\"running\"},{\"id\":\"${a_arch}\",\"status\":\"closed\"}]"
if out="$(paseo-ls-check)" ; then
    print -ru2 -- 'FAIL: paseo-ls-check accepted a mismatch' ; exit 1
fi
expect-eq "${out}" "error: missing from paseo-ls: ${a_arch}"$'\n'"error: extra in paseo-ls: ${a_idle}"$'\n''2 mismatched of 2 agents' 'paseo-ls-check mismatch'

# Scopes of sessions picked from another shell come from the transcript path.
typeset -gA claude_code_profile_homes=(default .claude work .claude-work)
expect-eq "$(h-agent-auto-continue-scope-of claude "${t}/.claude-work/projects/-p/x.jsonl")" claude-work 'Claude scope'
command mkdir -p -- "${t}/ch/sessions"
print -r -- a > "${t}/ch/auth.json" ; print -r -- a > "${t}/ch/auth_team.json"
expect-eq "$(h-agent-auto-continue-scope-of codex "${t}/ch/sessions/2026/x.jsonl")" codex-team 'Codex scope'
if h-agent-auto-continue-scope-of claude "${t}/.other/projects/x.jsonl" 2>/dev/null ; then
    print -ru2 -- 'FAIL: an unknown Claude home got a scope' ; exit 1
fi

# agent-auto-continue-fz: a Paseo row goes to the Paseo watcher, a native row
# is registered from outside.
typeset -g ac_log="${t}/ac.log"
function h-agent-session-live-rows {
    printf 'paseo:%s\t%s\tclaude\tlabel\n' "${a_live}" "${native_t}"
    printf 'w2\t%s\tclaude\tother\n' "${other_t}"
}
function h-agent-session-fz { command cat ; }
function h-agent-session-annotate-rows { command cat ; }
function paseo-auto-continue-on { print -r -- "paseo-on $*" >> "${ac_log}" ; }
function paseo-auto-continue-off { print -r -- "paseo-off $*" >> "${ac_log}" ; }
function h-agent-auto-continue-register-outside { print -r -- "register ${agent_auto_continue_frontmost_p} $*" >> "${ac_log}" ; }
: > "${ac_log}"
agent-auto-continue-fz --poll 90
expect-eq "$(<"${ac_log}")" "paseo-on ${a_live} --poll 90"$'\n'"register n claude ${other_t}" 'agent-auto-continue-fz dispatch'

# agent-auto-continue-off-fz: the native registration of a registered Paseo
# agent is not offered, and goes when the Paseo row does.
typeset -g agent_auto_continue_dir="${t}/ac"
command mkdir -p -- "${agent_auto_continue_dir}/claude-default" "${agent_auto_continue_dir}/claude-work"
printf 'claude\t%s\t%s\ttmux:%%1\t1\n' "${live_id}" "${native_t}" > "${agent_auto_continue_dir}/claude-default/claude-${live_id}"
printf 'claude\tplain\t\tkitty:4\t1\n' > "${agent_auto_continue_dir}/claude-work/claude-plain"
function h-paseo-auto-continue { [[ "${1}" == list ]] && printf '%s\t%s\tclaude/default\twaiting\n' "${a_live}" "${paseo_home}" ; }
function agent-auto-continue-check { : ; }
function h-agent-auto-continue-watch-stop { : ; }
function h-agent-session-fz { command tee "${t}/offered" | command grep -e '^paseo:' ; }
: > "${ac_log}"
agent-auto-continue-off-fz >/dev/null
expect-eq "$(command cut -f1 "${t}/offered" | command sort)" "paseo:${a_live}"$'\n'"reg:claude-work:claude:plain" 'off-fz offers each agent once'
expect-eq "$(<"${ac_log}")" "paseo-off ${a_live} --home ${paseo_home}" 'off-fz turns the Paseo watcher off'
if test -e "${agent_auto_continue_dir}/claude-default/claude-${live_id}" ; then
    print -ru2 -- 'FAIL: the hidden native registration survived' ; exit 1
fi
test -e "${agent_auto_continue_dir}/claude-work/claude-plain"

function h-agent-session-fz { command grep -e '^reg:' ; }
agent-auto-continue-off-fz >/dev/null
if test -e "${agent_auto_continue_dir}/claude-work/claude-plain" ; then
    print -ru2 -- 'FAIL: the picked native registration survived' ; exit 1
fi

# Registered children stay excluded even when off-fz would otherwise produce
# a fallback row, while an explicit opt-in can still remove the registration.
paseo-agents-test-record "${a_child}" claude "${child_id}" idle 'Workflow child'
command jq --arg parent "${a_live}" '.labels={"paseo.parent-agent-id":$parent}' \
    "${t}/home/agents/-p/${a_child}.json" > "${t}/child.json"
command mv -- "${t}/child.json" "${t}/home/agents/-p/${a_child}.json"
command mkdir -p -- "${agent_auto_continue_dir}/claude-default"
printf 'claude\t%s\t%s\ttmux:%%1\t1\n' "${child_id}" "${child_t}" \
    > "${agent_auto_continue_dir}/claude-default/claude-${child_id}"
function h-paseo-auto-continue {
    [[ "${1}" == list ]] && printf '%s\t%s\tclaude/default\twaiting\n' "${a_child}" "${paseo_home}"
}
function h-agent-session-fz { command tee "${t}/child-offered" ; }
agent-auto-continue-off-fz >/dev/null 2>&1 && { print -ru2 -- 'FAIL: hidden child was offered'; exit 1; }
test -e "${agent_auto_continue_dir}/claude-default/claude-${child_id}"
agent_session_fz_subagents_p=y agent-auto-continue-off-fz >/dev/null
expect-eq "$(command cut -f1 "${t}/child-offered")" "paseo:${a_child}" 'off-fz child opt-in'
test ! -e "${agent_auto_continue_dir}/claude-default/claude-${child_id}"
print -r -- 'Paseo agent row checks passed'
