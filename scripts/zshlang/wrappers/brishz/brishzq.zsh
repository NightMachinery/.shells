#!/usr/bin/env -S zsh -f
# macOS bug: https://stackoverflow.com/questions/9988125/shebang-pointing-to-script-also-having-shebang-is-effectively-ignored

if test "$1" = '-c' ; then
   shift
fi
##
path+=( /usr/local/bin /opt/homebrew/bin /home/linuxbrew/.linuxbrew/bin )
. ~/.privateShell
##
autoload -Uz regexp-replace

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

bool () {
    local i="${1:l}"
    if [[ "${i}" == (n|no|0) ]]
    then
        return 1
    else
        test -n "${i}"
        return $?
    fi
}

rgx () {
    local a
    (( $# == 2 )) && a="$(</dev/stdin)"  || {
        a="$1"
        shift 1
    }
    regexp-replace a "$1" "$2"
    print -r -- "$a"
}

isEmacs () {
    [[ -n "${NIGHT_EMACS_P}" ]]
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
local failure_expected="${brishz_failure_expected}"
local nolog="${brishz_nolog}"
local summary_p="${brishz_summary_p:-y}"
local endpoint="${bshEndpoint:-http://127.0.0.1:${GARDEN_PORT:-7230}}/zsh/"

#: @safety features that work around the upstream brish bug of not supporting binary IO and corrupting text
local out_from_file_p="${brishz_out_file_p}"
local eval_from_file_p="${brishz_eval_file_p}"

local input_cmd_raw=("$@")
local input_cmd=() out_file='' eval_file=''
if bool "$out_from_file_p" ; then
    out_file="$(mktemp)" || return $?
    input_cmd=(reval-out-to "$out_file")
fi
input_cmd+=( "${input_cmd_raw[@]}" )

if test -z "$brishz_noquote" ; then
    input_cmd="$(gq "${(@)input_cmd}")"

    local input_cmd_lines
    input_cmd_lines="$(gq "${input_cmd_raw[@]}")"
    input_cmd_lines=(${(@f)input_cmd_lines})

    if bool "$eval_from_file_p" ; then
        eval_file="$(mktemp)" || return $?
        local input_cmd_orig="$input_cmd"
        print -r -- "$input_cmd_orig" > "$eval_file" || return $?
        input_cmd=(source "$eval_file")
        input_cmd="$(gq "${(@)input_cmd}")"

        if bool "$summary_p" ; then
            if bool "$out_from_file_p" ; then
                input_cmd+=$'\n'"# out_file: $out_file"
            fi

           local input_cmd_summary
           # input_cmd_summary="${(pj|\n#   |)input_cmd_lines[1,3]}"
            input_cmd_summary="${(pj|\n#   |)input_cmd_lines[@]}"
            input_cmd+=$'\n'"# input_cmd_summary: $input_cmd_summary"
        fi
    fi

    local v forwarded_vars=(

    )

    if isEmacs ; then
        forwarded_vars+=(
            NIGHT_EMACS_P
            EMACS_SOCKET_NAME
            emacs_night_server_name
        )
        export EMACS_SOCKET_NAME="${emacs_night_server_name}"
        #: This var is somehow reset to its default in emacs?
    fi

    for v in ${forwarded_vars[@]} ; do
        if test -v "$v" ; then #: if var is set
            input_cmd="$(typeset -p "$v" | rgx "^export " "local -x ")"$'\n'"$input_cmd"
        fi
    done

    if [[ "$endpoint" =~ '^https?://127.0.0.1' ]] ; then
        # typeset -p input_cmd_lines
        input_cmd="( mark-me 'BRISHZQ_MARKER' $(gq "${input_cmd_raw[@]}")"$'\n'"cd $(gquote-sq "$PWD")"$'\n'"${input_cmd}"$'\n'"ret=\$? ; cd /tmp ; return-code \$ret )"
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
local req="$(print -nr -- "$stdin" \
    | jq --raw-input --slurp --null-input --compact-output \
    --arg nolog "$nolog" \
    --arg failure_expected "$failure_expected" \
    --arg s "$session" \
    --arg c "$input_cmd[*]" \
    --arg v $v \
    'inputs as $i | {"cmd": $c, "session": $s, "stdin": $i, "json_output": $v, "nolog": $nolog, "failure_expected": $failure_expected}')"
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
    if bool "$out_from_file_p" ; then
        cat "$out_file" || return $?
    else
        ec "$out" | jq -rje .out
    fi

    ec "$out" | jq -rje .err >&2

    exit "$(ec "$out" | jq -rje .retcode)"
else
    # ec "${0:t}: invalid json, assuming magic" >&2
    # @todo1 make garden output these in CmdResults as well

    ec "$out"
    return 200
fi
