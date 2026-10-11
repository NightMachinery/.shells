#!/usr/bin/env zsh
# Fixture-only checks for shared picker presentation and annotation.
setopt errexit pipefail typesetsilent
typeset -gr picker_root="${0:A:h:h:h}"
source "${picker_root}/zshlang/basic/basic.plugin.zsh"
function aliasfn { :; }
source "${picker_root}/zshlang/auto-load/others/agent-session.zsh"
setopt nounset
function expect-eq {
    [[ "${1}" == "${2}" ]] || { print -ru2 -- "FAIL: ${3}: ${(qqq)1}"; exit 1; }
}

typeset -g id_cmd="$(h-agent-session-fz-id-cmd)"
# fzf substitutes numeric braces before running a transform command. A Perl
# quantifier here would pass a plain shell test but fail in the real picker.
[[ "${id_cmd}" != *\{[0-9]* ]] || { print -ru2 -- 'FAIL: unintended fzf field placeholder'; exit 1; }
typeset -g uuid=deadbeef-1111-4aaa-8bbb-cccccccccccc
typeset -g q
for q in "${uuid}" deadbeef deadbeef-a "paseo:${uuid}" "'${uuid}" "research ${uuid}" "${uuid:u}" ; do
    expect-eq "$(FZF_QUERY="${q}" FZF_WITH_NTH=4.. command dash -c "${id_cmd}")" \
        'change-with-nth(1,4..)' "ID query: ${q}"
done
for q in '' research deadbee 'research-deadbeefish' 'printf $(printf inert)' ; do
    expect-eq "$(FZF_QUERY="${q}" FZF_WITH_NTH=1,4.. command dash -c "${id_cmd}")" \
        'change-with-nth(4..)' "title query: ${q}"
done
expect-eq "$(FZF_QUERY="${uuid}" FZF_WITH_NTH=1,4.. command dash -c "${id_cmd}")" '' 'no redundant refresh'

function h-agent-session-dep { :; }
function h-agents { print -r -- claude; }
function h-agent-field { print -r -- C; }
function h-agent-session-call {
    case "${2}" in
        roots) print -r -- /fixtures ;;
        owns-p) [[ "${3}" == /fixtures/* ]] ;;
    esac
}
function agent_session {
    printf '100\t/fixtures/old.jsonl\told stamp\tOld\told.jsonl\told snippet\n'
    printf '300\t/fixtures/new.jsonl\tnew stamp\tNew\tnew.jsonl\tnew snippet\n'
}
typeset -g pairs=$'old\t/fixtures/old.jsonl\tclaude\t-\nnew\t/fixtures/new.jsonl\tclaude\t-\npaseo:fallback\t-\t-\tFallback\tbadge\t200\tfallback stamp\nmissing\t-\t-\tMissing'
unset agent_session_rows_sort
typeset -g rows="$(print -r -- "${pairs}" | h-agent-session-annotate-rows)"
expect-eq "$(print -r -- "${rows}" | command cut -f1)" $'new\npaseo:fallback\nold\nmissing' 'newest first with fallback'
expect-eq "$(print -r -- "${rows}" | command sed -n '2p' | command cut -f6)" 'fallback stamp' 'fallback timestamp displayed'
expect-eq "$(print -r -- "${rows}" | command awk -F'\t' 'NF != 8 { print NF }')" '' 'eight columns remain intact'
expect-eq "$(print -r -- "${pairs}" | agent_session_rows_sort='' h-agent-session-annotate-rows | command cut -f1)" \
    $'old\nnew\npaseo:fallback\nmissing' 'caller order opt-out'
print -r -- 'Session picker presentation and sorting checks passed'
