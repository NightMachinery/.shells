#!/usr/bin/env -S zsh -f
# macOS bug: https://stackoverflow.com/questions/9988125/shebang-pointing-to-script-also-having-shebang-is-effectively-ignored

if test "$1" = '-c' ; then
   shift
fi

path+=( /usr/local/bin )
. ~/.privateShell

alias ec='print -r --'
function gquote() {
    ec "${(q+@)@[1]}" "${(qq@)@[2,-1]}"
}
alias gq=gquote

function gquote-sq() {
    # uses single-quotes
    ec "${(qq@)@}"
}

isDbg () {
    test -n "$DEBUGME"
}

##
# typeset -a gray=( 170 170 170 )
# ecgray () {
#     {
# ;45;10M64;45;10M        [ -t 2 ] && colorfg "$gray[@]"
#         print -r -- "${@}"
#         [ -t 2 ] && resetcolor
#     } >&2
# }
# isI () {
#     true
#     ##
#     # test -z "$FORCE_NONINTERACTIVE" && {
#     #     test -n "$FORCE_INTERACTIVE" || [[ -o interactive ]]
#     # }
# }
# colorfg () {
#     ! isI || printf "\x1b[38;2;${1:-0};${2:-0};${3:-0}m"
# }
# resetcolor () {
#     ! isI || printf %s $'\C-[[00m'
# }
###
local copy_cmd="$brishz_copy"
local session="${brishz_session}"
local nolog="${brishz_nolog}"
local endpoint="${bshEndpoint:-http://127.0.0.1:$GARDEN_PORT}/zsh/"

local input_cmd=( "$@" )
if test -z "$brishz_noquote" ; then
    input_cmd="$(gq "${(@)input_cmd}")"

    if [[ "$endpoint" =~ '^https?://127.0.0.1' ]] ; then
        input_cmd="cd $(gquote-sq "$PWD") ; ${input_cmd} ; ret=\$? ; cd /tmp ; (exit \$ret) "
    fi
fi

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
if test -n "$nolog" ; then
    endpoint+="nolog/"
fi
if [[ "$endpoint" =~ 'garden' ]] ; then
    opts+=(--user "Alice:$GARDEN_PASS0")
fi
local v=1
local req="$(print -nr -- "$stdin" | jq --raw-input --slurp --null-input --compact-output --arg nolog "$nolog" --arg s "$session" --arg c "$input_cmd[*]" --arg v $v 'inputs as $i | {"cmd": $c, "session": $s, "stdin": $i, "json_output": $v, "nolog": $nolog}')"
local cmd=( curl $opts[@] --fail --silent --location --header "Content-Type: application/json" --request POST --data '@-' $endpoint )
cmd="$(gq print -nr -- $req) | $(gq "$cmd[@]")"
if ((${+commands[pbcopy]})) ; then
    if test -n "$copy_cmd" ; then
        <<<"$cmd" pbcopy
    fi
fi
local out
out="$(eval "$cmd")" || return $?

if ec "$out" | jq -e . >/dev/null 2>&1 ; then
    ec "$out" | jq -rje .out
    ec "$out" | jq -rje .err >&2
    exit "$(ec "$out" | jq -rje .retcode)"
else
    # ec "${0:t}: invalid json, assuming magic" >&2
    # @todo1 make garden output these in CmdResults as well

    ec "$out"
    return 200
fi
