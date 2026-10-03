#!/usr/bin/env -S zsh -f
# Wait for actual ZLE readiness, never a timing guess. The caller bounds runtime.
zmodload zsh/zpty
local root="${${(%):-%x}:A:h:h:h:h}"
local output all=''
function wait-for {
    local wanted="$1"
    while [[ "$all" != *"$wanted"* ]] ; do
        zpty -r widget output '*' || return 1
        all+="$output"
        if [[ -n "$LLM_COMPLETE_TEST_TRACE" ]] ; then print -ru2 -- "${(V)output}" ; fi
    done
    all="${all#*${wanted}}"
}
export TERM=xterm-256color
zpty widget /bin/zsh -dfi
wait-for $'\e[?2004h' || exit 1
local setup="source ${(q)root}/zshlang/basic/basic.plugin.zsh; source ${(q)root}/zshlang/basic/colors.zsh; source ${(q)root}/zshlang/basic/conditions-personal.zsh; source ${(q)root}/zshlang/basic/proxy.zsh; source ${(q)root}/zshlang/auto-load/others/fim.zsh; source ${(q)root}/zshlang/interactive/auto-load/FIM.zsh; PROMPT='FIMTEST> '; RPROMPT=''; bindkey -v; fim_proxy_p=n; fim_provider=${LLM_COMPLETE_TEST_PROVIDER:-stub}; function h-test-buffer { print -r -- \"SNAPSHOT=\$BUFFER\"; zle redisplay }; zle -N h-test-buffer; bindkey '^G' h-test-buffer"
zpty -w widget "$setup"
wait-for 'FIMTEST> ' || exit 1
wait-for $'\e[?2004h' || exit 1
zpty -w -n widget 'count ='
zpty -w -n widget $'\e.'
wait-for 'inserted' || exit 1
zpty -w -n widget $'\C-g'
wait-for 'SNAPSHOT=count = 0' || exit 1
print -r -- 'PASS: widget inserted exact leading-space completion'
zpty -w -n widget $'\C-u'
zpty -w widget 'fim_provider=nope'
wait-for $'\e[?2004h' || exit 1
zpty -w -n widget 'inert'
zpty -w -n widget $'\e.'
wait-for "unknown provider 'nope'" || exit 1
print -r -- 'PASS: widget showed unknown-provider error'
zpty -d widget
