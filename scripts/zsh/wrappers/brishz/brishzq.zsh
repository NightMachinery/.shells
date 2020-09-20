#!/usr/bin/env zshplain.dash

. ~/.privateShell
alias gq=gquote
gquote () {
    print -r -- "${(q+@)@}"
}
isDbg () {
    test -n "$DEBUGME"
}
###
local copy_cmd="$brishz_copy"
local input_cmd=( "$@" )
test -z "$brishz_noquote" && input_cmd="$(gq "${(@)input_cmd}")"
local stdin="$brishz_in"
[[ "$stdin" == 'MAGIC_READ_STDIN' ]] && stdin="$(</dev/stdin)"


local opts=()
## old GET
# isDbg && opts+=(--data 'verbose=1')
# rgeval curl --silent -G $opts[@] --data-urlencode "cmd=$(gq "$@")" http://127.0.0.1:8000/zsh/
##
# httpie is slow
# isDbg && opts+=(verbose=1)
# http --body POST http://127.0.0.1:8000/zsh/ cmd="$(gq "$@")" $opts[@]
##
local endpoint="${bshEndpoint:-http://127.0.0.1:$GARDEN_PORT}"
if [[ "$endpoint" =~ 'garden' ]] ; then
    opts+=(--user "Alice:$GARDEN_PASS0")
fi
local v=0
isDbg && v=1
local req="$(print -nr -- "$stdin" | jq --raw-input --slurp --null-input --compact-output --arg c "$input_cmd[*]" --arg v $v 'inputs as $i | {"cmd": $c, "stdin": $i, "verbose": $v}')"
local cmd=( curl $opts[@] --fail --silent --location --header "Content-Type: application/json" --request POST --data '@-' $endpoint/zsh/ )
cmd="$(gq print -nr -- $req) | $(gq "$cmd[@]")"
if ((${+commands[pbcopy]})) ; then
    test -n "$copy_cmd" && <<<"$cmd" pbcopy
fi
eval "$cmd"
