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
rows="$(h-paseo-agent-rows | command sort)"
expect-eq "${rows}" "${a_live}"$'\t'claude$'\t'"${live_id}"$'\t'running$'\t'p-claude$'\t''Live one'$'\n'"${a_idle}"$'\t'codex$'\t'"${idle_id}"$'\t'idle$'\t'p-codex$'\t''Idle one' \
    'rows from stored records'

# The filter drops exactly the native row a Paseo agent runs.
kept="$(printf 'w1\t%s\tclaude\tnative\nw2\t%s\tclaude\tother\n' "${native_t}" "${other_t}" | h-paseo-filter-pairs)"
expect-eq "${kept}" "w2"$'\t'"${other_t}"$'\t'claude$'\t'other 'filter drops the native row'

# Pairs: the live agent's transcript from the listing, the idle one's by glob.
pairs="$(h-paseo-agent-pairs paseo: | command sort)"
expect-eq "${pairs}" "paseo:${a_live}"$'\t'"${native_t}"$'\t'claude$'\t'"paseo:${a_live[1,8]}  Live one (running)"$'\t'"${paseo_agent_badge}"$'\n'"paseo:${a_idle}"$'\t'"${idle_t}"$'\t'codex$'\t'"paseo:${a_idle[1,8]}  Idle one (idle)"$'\t'"${paseo_agent_badge}" \
    'picker pairs'

# A remote daemon is refused, and the filter then passes everything through.
if PASEO_HOST=remote:1 h-paseo-agent-rows 2>/dev/null ; then
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
exit 0
SH
command chmod 700 "${t}/bin/paseo"
path=("${t}/bin" "${path[@]}")
typeset -gx PASEO_TEST_LOG="${t}/paseo.log" PASEO_TEST_INSPECT=''

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

print -r -- 'Paseo agent row checks passed'
