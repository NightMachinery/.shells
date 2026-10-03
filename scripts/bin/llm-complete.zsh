#!/usr/bin/env -S zsh -f
# Runtime environment for every non-zsh FIM front end. Request JSON is stdin.
local root="${${(%):-%x}:A:h:h}"
source "${root}/zshlang/basic/basic.plugin.zsh" || exit 70
source "${root}/zshlang/basic/colors.zsh" || exit 70
source "${root}/zshlang/basic/conditions-personal.zsh" || exit 70
source "${root}/zshlang/basic/proxy.zsh" || exit 70
path=("$HOME/go/bin" "$HOME/bin" /opt/homebrew/bin $path)
if [[ -r ~/.privateShell ]] ; then
    source ~/.privateShell || exit 70
fi
go-local-dep llm_complete "${root}/golang/llm_complete" || exit 70
#: Export keys by metadata, using shell builtins. Values never become argv.
local key
while IFS= read -r key ; do
    [[ -n "$key" ]] && typeset -x "$key=${(P)key}"
done < <(command llm_complete fim providers --json | command jq -r '.[].key_env | select(length > 0)' | command sort -u)
if should-proxy-p ; then pxa-local ; fi
exec command llm_complete "$@"
