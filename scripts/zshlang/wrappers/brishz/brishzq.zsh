#!/usr/bin/env -S zsh -f
# macOS bug: https://stackoverflow.com/questions/9988125/shebang-pointing-to-script-also-having-shebang-is-effectively-ignored

if [[ "${DISABLE_BRISH:l}" == y ]] ; then
    echo "brishq.zsh: disabled by DISABLE_BRISH" >&2
    exit 1
fi

if test "$1" = '-c' ; then
   shift
fi
##
function psource {
    #: @duplicateCode/9ae25a72d39d3e69299cc8b3eb6310c9
    ##
    if [[ -r "$1" ]]; then # -r: readable file
        source "$@"
    fi
}
##
path+=( /usr/local/bin /opt/homebrew/bin /home/linuxbrew/.linuxbrew/bin )
psource ~/.privateShell
##
autoload -Uz regexp-replace

alias ec='print -r --'
alias ecn='print -rn --'

local debug_p=''
# debug_p=y

test -n "${debug_p}" && ec 'brishzq.zsh: started'

function gquote() {
    #: The first word stays bare when it is made of safe characters only
    #: (ASCII letters, digits and `_ . / , : @ % + -`, not starting with `=`
    #: or `~`), so a command name reads as itself. Anything else is
    #: single-quoted like the other words. It used `(q+)`, which mis-escapes
    #: some bytes and code points (an ideographic space, for one, stayed
    #: bare), so a crafted first word could inject code.
    if (( $# == 0 )) ; then
        ec "''"
    elif h-brishzq-safe-word-p "$1" ; then
        ec "$1" "${(qq@)@[2,-1]}"
    else
        ec "${(qq)1}" "${(qq@)@[2,-1]}"
    fi
}

function h-brishzq-safe-word-p {
    : "usage: h-brishzq-safe-word-p <word>
Succeeds when <word> needs no quoting as the first word of a command."
    #: Byte by byte, so no locale's idea of a letter or a range can widen it.
    setopt localoptions nomultibyte
    [[ -n "$1" && "$1" != *[^A-Za-z0-9_./,:@%+-]* && "$1" != [=~]* ]]
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
function h-brishzq-b64-payloads {
    : "usage: h-brishzq-b64-payloads <cmd> <stdin>
Prints base64(<cmd>), a '.', then base64(<stdin>); the <stdin> MAGIC_READ_STDIN reads our own stdin."
    #: For brishz_binary: jq is not binary-safe (it turns invalid UTF-8 into
    #: U+FFFD), so the raw bytes pass through the base64 binary before jq
    #: sees them. They go through a pipe, never argv, which is size-limited
    #: and shown by `ps`. The '.' is outside the base64 alphabet. Any line
    #: breaks the encoder adds are fine: the garden ignores whitespace there.
    setopt localoptions pipefail
    local cmd="$1" stdin="$2"

    print -rn -- "$cmd" | command base64 || return $?
    print -rn -- '.'
    if [[ "$stdin" == 'MAGIC_READ_STDIN' ]] ; then
        command base64 || return $?
    else
        print -rn -- "$stdin" | command base64 || return $?
    fi
}

function h-brishzq-binary-header-p {
    : "usage: h-brishzq-binary-header-p <header-file>
Succeeds when the headers curl dumped there include X-Brish-Binary: 1."
    #: curl dumps the headers of every response it got (a redirect's, a
    #: 100 Continue's), with CRLF line ends. Header names are
    #: case-insensitive, and the garden sends this one lowercased.
    local header_file="$1" line

    for line in "${(@f)$(<"$header_file")}" ; do
        line="${line%$'\r'}"
        if [[ "${line:l}" =~ '^x-brish-binary:[[:space:]]*1[[:space:]]*$' ]] ; then
            return 0
        fi
    done
    return 1
}

function h-brishzq-urlencode {
    : "usage: h-brishzq-urlencode <string>
Sets REPLY to <string> percent-encoded byte by byte, for a URL's query."
    setopt localoptions nomultibyte
    local s="$1" c out=''
    integer i

    for (( i = 1 ; i <= ${#s} ; i++ )) ; do
        c="${s[i]}"
        if [[ "$c" == [A-Za-z0-9._~-] ]] ; then
            out+="$c"
        else
            printf -v c '%%%02X' "$(( #c ))"
            out+="$c"
        fi
    done
    REPLY="$out"
}

function h-brishzq-print-notice {
    : "usage: h-brishzq-print-notice <text>
Prints a reply that is not a command's result, as the JSON path always has:
without its trailing newlines, plus one newline."
    local text="$1"

    while [[ "$text" == *$'\n' ]] ; do
        text="${text%$'\n'}"
    done
    ec "$text"
}

typeset -ga brishzq_tmp_files=()
function h-brishzq-cleanup {
    : "Removes the temp files this run created (brishzq_tmp_files)."
    if (( ${#brishzq_tmp_files} )) ; then
        command rm -f -- "${brishzq_tmp_files[@]}" || true
    fi
}

function h-brishzq-tmp {
    : "usage: h-brishzq-tmp
Sets REPLY to a new temp file, which is removed when the script exits."
    #: The EXIT trap is set at the top level, since one set inside a
    #: function fires when that function returns. The signal traps are set
    #: here, at the first temp file, so a run without temp files (the raw
    #: path) keeps the default signal handling. A signal removes the files
    #: and then kills us with that same signal, so our parent still sees it.
    if (( ${#brishzq_tmp_files} == 0 )) ; then
        local sig
        for sig in HUP INT TERM ; do
            trap "h-brishzq-cleanup ; brishzq_tmp_files=() ; trap - $sig EXIT ; kill -$sig \$\$" "$sig"
        done
    fi
    REPLY="$(command mktemp)" || return $?
    brishzq_tmp_files+=( "$REPLY" )
}

function h-brishzq-raw {
    : "usage: h-brishzq-raw <cmd> <stdin>
Runs <cmd> with <stdin> through the garden's raw API (POST .../zsh/raw/),
prints its stdout and stderr exactly, and exits with its status. Returns (0)
only when the garden has no raw API (HTTP 404 or 405), which then ran nothing."
    #: Reads the script's locals: raw_endpoint, session, nolog,
    #: failure_expected, opts, copy_cmd, debug_p.
    #:
    #: Byte semantics throughout: X-Brish-Cmd-Length counts bytes, and the
    #: reply is split at byte offsets.
    setopt localoptions nomultibyte
    local cmd="$1" stdin="$2"
    local url="$raw_endpoint"
    local -a query=()

    if test -n "$session" ; then
        h-brishzq-urlencode "$session"
        query+=( "session=${REPLY}" )
    fi
    #: The JSON path sends these as strings, and the garden takes any
    #: non-empty string there as true.
    if test -n "$nolog" ; then
        query+=( nolog=1 )
    fi
    if test -n "$failure_expected" ; then
        query+=( failure_expected=1 )
    fi
    if (( ${#query} )) ; then
        url+="?${(j:&:)query}"
    fi

    #: No `--fail`: the HTTP status tells an old garden (404) from an error.
    #: `--dump-header -` puts every response's headers (a 100 Continue's
    #: too) before the body on stdout, and `size_header` counts them, so the
    #: body is split off by length, whatever bytes it holds. `Expect:` stops
    #: curl from waiting for a 100 Continue on large bodies.
    local -a curl_cmd=( command curl "${opts[@]}" --silent --location
        --header 'Content-Type: application/octet-stream' --header 'Expect:'
        --header "X-Brish-Cmd-Length: ${#cmd}"
        --data-binary '@-' --dump-header - --write-out $'\n%{size_header} %{http_code}'
        "$url" )

    test -n "${debug_p}" && ec "brishzq.zsh: raw: ${url}, command bytes: ${#cmd}, stdin bytes: ${#stdin}"
    if test -n "$copy_cmd" && ((${+commands[pbcopy]})) ; then
        <<<"$(gq print -rn -- "$cmd$stdin") | $(gq "${curl_cmd[@]}")" pbcopy
    fi

    local reply
    reply="$(print -rn -- "$cmd$stdin" | "${curl_cmd[@]}")"
    local ret=$?
    if (( ret != 0 )) ; then
        #: A transport failure: curl's own code, as on the JSON path
        #: (Hammerspoon reads 7 as "not sent").
        exit "$ret"
    fi

    local wo="${reply[-40,-1]}"
    wo="${wo##*$'\n'}"
    local size_header="${wo% *}" http_code="${wo##* }"
    if ! [[ "$size_header" == <-> && "$http_code" == <-> ]] ; then
        ec "brishzq.zsh: could not parse curl's reply" >&2
        exit 1
    fi
    reply="${reply[1,-$(( ${#wo} + 2 ))]}"

    if (( http_code == 404 || http_code == 405 )) ; then
        test -n "${debug_p}" && ec "brishzq.zsh: raw: HTTP ${http_code}, falling back to the JSON API"
        return 0
    fi
    if (( http_code >= 400 )) ; then
        #: The JSON path's `curl --fail`: exit 22, print nothing.
        exit 22
    fi

    local headers="${reply[1,size_header]}" body="${reply[size_header+1,-1]}"
    local line name value retcode='' out_length='' notice_p='' binary_mode=''
    for line in "${(@ps:\r\n:)headers}" ; do
        if [[ "$line" == HTTP/* ]] ; then
            #: A new response (after a 100 Continue or a redirect).
            retcode='' out_length='' notice_p='' binary_mode=''
            continue
        fi
        name="${${line%%:*}:l}"
        value="${line#*:}"
        while [[ "$value" == [[:blank:]]* ]] ; do value="${value#?}" ; done
        while [[ "$value" == *[[:blank:]] ]] ; do value="${value%?}" ; done
        case "$name" in
            (x-brish-retcode) retcode="$value" ;;
            (x-brish-out-length) out_length="$value" ;;
            (x-brish-notice) [[ "$value" == 1 ]] && notice_p=y ;;
            (x-brish-binary) binary_mode="$value" ;;
        esac
    done

    if test -n "$notice_p" ; then
        #: A notice (a magic command's log, an empty command): printed and
        #: exit 200, as the JSON path does.
        h-brishzq-print-notice "${body[1,${out_length:-${#body}}]}"
        exit 200
    fi
    if ! [[ "$retcode" == (-|)<-> && "$out_length" == <-> ]] || (( out_length > ${#body} )) ; then
        #: Not the garden's raw reply. The JSON path prints a reply that is
        #: not a command's result and exits 200.
        h-brishzq-print-notice "$body"
        exit 200
    fi

    if [[ "$binary_mode" == 0 && "$retcode" == 9000 && "$out_length" == 0 ]] \
        && [[ "$body" == ('brishgarden: '|'Illegal input: ')* ]] \
        && [[ "$cmd$stdin" == *[$'\0'$'\x80'-$'\xff']* ]] ; then
        #: A garden in legacy (text) mode refuses a command or stdin that is
        #: not valid UTF-8 or holds a NUL, before running anything. The JSON
        #: path runs those as it always has (stdin from a temp file arrives
        #: exact), so send the request there.
        test -n "${debug_p}" && ec "brishzq.zsh: raw: refused by a legacy-mode garden, falling back to the JSON API"
        return 0
    fi

    print -rn -- "${body[1,out_length]}"
    print -rn -- "${body[out_length+1,-1]}" >&2
    exit "$retcode"
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
local endpoint_base="${bshEndpoint:-http://127.0.0.1:${GARDEN_PORT:-7230}}"
local endpoint="${endpoint_base}/zsh/"
#: brishz_binary=y: exact bytes both ways or nothing runs, over the JSON
#: API's binary transport (cmd_b64, stdin_b64, binary: 1); see
#: docs/brishz-binary.md. It needs a garden in binary mode.
local binary_p="${brishz_binary}"

#: @safety features that work around the upstream brish bug of not supporting binary IO and corrupting text
#: The raw API and brishz_binary=y supersede them; they stay for gardens without binary support.
local out_from_file_p="${brishz_out_file_p}"
local eval_from_file_p="${brishz_eval_file_p}"

#: brishz_raw=y: send the request to the garden's raw API (POST
#: /zsh/raw/), which carries the command, stdin, stdout and stderr as exact
#: bytes, with no JSON, and about halves the client's time per call. A
#: garden without it answers 404 and runs nothing; we then send the same
#: request to the JSON API, which costs about 25 ms more per call. So it
#: stays opt-in until the running garden has the raw API; see
#: docs/brishz-raw.md. The options above that need the JSON API
#: (brishz_binary, brishz_out_file_p, brishz_eval_file_p) always use it.
local raw_p="${brishz_raw:-n}"
if bool "$binary_p" || bool "$out_from_file_p" || bool "$eval_from_file_p" ; then
    raw_p=n
fi

trap 'h-brishzq-cleanup' EXIT

local input_cmd_raw=("$@")
local input_cmd=() out_file='' eval_file=''
if bool "$out_from_file_p" ; then
    h-brishzq-tmp || exit $?
    out_file="$REPLY"
    input_cmd=(reval-out-to "$out_file")
fi
input_cmd+=( "${input_cmd_raw[@]}" )

if test -z "$brishz_noquote" ; then
    input_cmd="$(gq "${(@)input_cmd}")"

    local input_cmd_lines
    input_cmd_lines="$(gq "${input_cmd_raw[@]}")"
    input_cmd_lines=(${(@f)input_cmd_lines})

    if bool "$eval_from_file_p" ; then
        h-brishzq-tmp || exit $?
        eval_file="$REPLY"
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


local stdin="${brishz_in}"
local stdin_file_p=''
if bool "$binary_p" ; then
    #: h-brishzq-b64-payloads streams stdin into base64 and it travels in
    #: the request as stdin_b64, so it also reaches a remote garden, which
    #: cannot read our temp files.
    test -n "${debug_p}" && ec "brishzq.zsh: binary mode, stdin: ${stdin}"
elif [[ "$stdin" == 'MAGIC_READ_STDIN' ]] ; then
    test -n "${debug_p}" && ec 'brishzq.zsh: reading stdin'

    stdin_file_p='y'

    stdin="${$(</dev/stdin ; ecn .)[1,-2]}"
    # stdin="$(cat)"

    test -n "${debug_p}" && ec 'brishzq.zsh: stdin read'

else
    test -n "${debug_p}" && ec "brishzq.zsh: stdin: ${stdin}"
fi



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
#: Local requests authenticate with the API key; remote ones go through Caddy,
#: which injects the garden's own key after checking basic auth.
#: `--header @file` keeps the key out of argv, which `ps` exposes to other local users.
local apikey_file="$HOME/.keys/brishgarden"
if [[ "$endpoint" =~ '^https?://(127\.0\.0\.1|localhost)' ]] && [[ -r "$apikey_file" ]] ; then
    opts+=(--header "@${apikey_file}")
fi
if bool "$raw_p" ; then
    local raw_endpoint="${endpoint_base}/zsh/raw/"
    if test -n "$nolog" ; then
        raw_endpoint+="nolog/"
    fi
    #: Exits, except when the garden has no raw API.
    h-brishzq-raw "$input_cmd[*]" "$stdin"
fi

local v=1
local req

if bool "$binary_p" ; then
    #: The command goes only in cmd_b64, never also in cmd: a garden that
    #: predates the _b64 fields then gets an empty command and runs nothing.
    #: `setopt` inside `$(...)` stays in that subshell.
    req="$(setopt pipefail
        h-brishzq-b64-payloads "$input_cmd[*]" "$stdin" \
            | command jq --raw-input --slurp --compact-output \
            --arg nolog "$nolog" \
            --arg failure_expected "$failure_expected" \
            --arg s "$session" \
            --arg v $v \
            'split(".") as $p | {"cmd_b64": $p[0], "stdin_b64": $p[1], "binary": 1, "session": $s, "json_output": $v, "nolog": $nolog, "failure_expected": $failure_expected}')" || {
        ec "brishzq.zsh: could not encode the binary request" >&2
        exit 1
    }
elif test -n "${stdin_file_p}" ; then
    local stdin_f
    h-brishzq-tmp || {
        ec "Failed to create temporary file for stdin." >&2
        exit 1
    }
    stdin_f="$REPLY"
    ecn "$stdin" > "$stdin_f"
req="$(jq --null-input --compact-output \
    --arg nolog "$nolog" \
    --arg failure_expected "$failure_expected" \
    --arg s "$session" \
    --arg c "< $(gquote-sq "${stdin_f}") {"$'\n'"$input_cmd[*]"$'\n'"}" \
    --arg v $v \
    '{"cmd": $c, "session": $s, "json_output": $v, "nolog": $nolog, "failure_expected": $failure_expected}')"
else
req="$(print -nr -- "$stdin" \
    | jq --raw-input --slurp --null-input --compact-output \
    --arg nolog "$nolog" \
    --arg failure_expected "$failure_expected" \
    --arg s "$session" \
    --arg c "$input_cmd[*]" \
    --arg v $v \
    'inputs as $i | {"cmd": $c, "session": $s, "stdin": $i, "json_output": $v, "nolog": $nolog, "failure_expected": $failure_expected}')"
fi

if bool "$binary_p" ; then
    #: A garden with binary support marks its reply with X-Brish-Binary: 1.
    #: Any other garden has refused or ignored the request, so without the
    #: header we fail rather than print its reply as the command's output.
    #: Every reply is HTTP 200, so `--fail` cannot tell.
    local header_file
    h-brishzq-tmp || exit $?
    header_file="$REPLY"
    local curl_cmd=( command curl $opts[@] --fail --silent --location --dump-header "$header_file" --header "Content-Type: application/json" --request POST --data-binary '@-' $endpoint )

    test -n "${debug_p}" && ec "brishzq.zsh: req: ${req}"
    if test -n "$copy_cmd" && ((${+commands[pbcopy]})) ; then
        <<<"$(gq print -nr -- $req) | $(gq "$curl_cmd[@]")" pbcopy
    fi

    local out ret header_p=''
    out="$(print -rn -- "$req" | "$curl_cmd[@]")"
    ret=$?
    if h-brishzq-binary-header-p "$header_file" ; then
        header_p=y
    fi
    command rm -f -- "$header_file" || true
    if (( ret != 0 )) ; then
        exit "$ret"
    fi
    if test -z "$header_p" ; then
        ec "brishzq.zsh: garden lacks binary support (no X-Brish-Binary header); restart the garden process with BRISH_BINARY=1" >&2
        exit 201
    fi

    #: `--exit-status`, since some jq builds exit 0 on a parse error.
    local fields_str
    local -a fields
    if fields_str="$(ec "$out" | command jq --exit-status --raw-output --join-output 'if (.out_b64|type) == "string" and (.err_b64|type) == "string" and (.retcode|type) == "number" then "\(.out_b64) \(.err_b64) \(.retcode)" else error("not a binary CmdResult") end' 2>/dev/null)" ; then
        fields=( "${(@s: :)fields_str}" )
        if bool "$out_from_file_p" ; then
            command cat -- "$out_file" || exit $?
        else
            print -rn -- "$fields[1]" | command base64 --decode || exit $?
        fi

        print -rn -- "$fields[2]" | command base64 --decode >&2

        exit "$fields[3]"
    else
        #: Not a command's result: a notice, such as a magic command's log.
        ec "$out"
        exit 200
    fi
fi

local cmd=( curl $opts[@] --fail --silent --location --header "Content-Type: application/json" --request POST --data '@-' $endpoint )

test -n "${debug_p}" && ec "brishzq.zsh: req: ${req}"
test -n "${debug_p}" && ec "brishzq.zsh: cmd: ${cmd}"

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
