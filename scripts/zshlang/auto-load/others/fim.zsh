##
#: Fill-in-the-middle (FIM) completion: hand a model the text before the
#: cursor and the text after it, get back the missing middle.
#:
#: This file is the composable half -- [agfi:fim-get] works in any shell, in a
#: pipe, or over brish. The interactive half, an =alt+.= widget, lives in
#: =zshlang/interactive/auto-load/FIM.zsh= and is only loaded in interactive
#: shells.
#:
#: The shared transport and provider table live in =golang/llm_complete=.
#: Emacs and Hammerspoon use the same binary.
#:
#: See =docs/fim.md=.
##
#: Every native FIM API takes an identical body -- model, prompt, suffix,
#: max_tokens, stop, temperature -- so a provider is just four strings.
##
typeset -gA fim_provider_endpoint fim_provider_model fim_provider_key_var fim_provider_extract

function h-llm-complete-dep {
    go-local-dep llm_complete "${NIGHTDIR:-$HOME/scripts}/golang/llm_complete"
}

function h-fim-providers-refresh {
    h-llm-complete-dep || return
    local row name model endpoint key extract
    local metadata="$(command llm_complete fim providers --json)" || return
    fim_provider_endpoint=() fim_provider_model=() fim_provider_key_var=() fim_provider_extract=()
    while IFS=$'\t' read -r name model endpoint key extract ; do
        fim_provider_endpoint[$name]="$endpoint"
        fim_provider_model[$name]="$model"
        fim_provider_key_var[$name]="${key:#-}"
        if [[ "$extract" == chat ]] ; then
            fim_provider_extract[$name]='.choices[0].message.content'
        else
            fim_provider_extract[$name]='.choices[0].text'
        fi
    done < <(print -r -- "$metadata" | command jq -r '.[] | [.name,.model,.endpoint,(if .key_env == "" then "-" else .key_env end),.extract] | @tsv')
}

#: The widget reads these metadata arrays; the provider definitions live in Go.
h-fim-providers-refresh
typeset -g fim_provider="${fim_provider:-$(command llm_complete fim default-provider)}"
##
function fim-providers {
    #: Names usable as `fim_provider'.
    h-llm-complete-dep || return
    command llm_complete fim providers
}

function fim-provider-show {
    ec "${fim_provider} (${fim_provider_model[${fim_provider}]})"
}

function fim-provider-select {
    #: Change the default provider for this shell.
    local chosen
    chosen="$(fim-providers | fz)" @TRET

    if test -z "${chosen}" ; then
        return 1
    fi

    typeset -g fim_provider="${chosen}"
    fim-provider-show
}
##
function h-fim-error-message {
    #: Render an API error body as one readable line.
    #:
    #: Mistral puts the message in `detail' for auth and validation failures
    #: and in `message' elsewhere; DeepSeek uses the OpenAI-shaped
    #: `error.message'. A 502 from a load balancer is HTML and has none of
    #: them, hence the fallback.
    setopt localoptions extendedglob

    local body="${1}"
    local max_len="${2:-200}"

    local msg=''
    if test -n "${body}" ; then
        msg="$(print -r -- "${body}" |
            jq --raw-output '
                if type == "object"
                then (.detail // .message // .error.message // empty)
                else empty end |
                if type == "string" then . else tojson end' 2>/dev/null)"
    fi

    if test -z "${msg}" ; then
        msg="${body}"
    fi

    #: Collapse to a single line; this ends up in `zle -M', which has one.
    msg="${${msg//[$'\n\r\t']/ }##[[:space:]]#}"
    msg="${msg%%[[:space:]]#}"

    if (( ${#msg} > max_len )) ; then
        msg="${msg[1,${max_len}]}…"
    fi

    if test -z "${msg}" ; then
        msg='(empty response body)'
    fi

    print -r -- "${msg}"
}
##
function fim-get-v1 {
    #: Usage: fim-get <prefix> [<suffix>]
    #:
    #: Prints the completion with no trailing newline (unless stdout is a tty),
    #: so that a caller can splice it in verbatim.
    #:
    #: Keyword arguments, all namespaced `fim_':
    #:   fim_provider     one of [agfi:fim-providers]; default codestral
    #:   fim_model        override the provider's model
    #:   fim_max_tokens   default 64
    #:   fim_stop         default a newline; empty to disable
    #:   fim_temperature  default 0
    #:   fim_timeout      seconds, default 20
    #:   fim_proxy_p      default y, and a no-op unless a proxy is configured
    #:   fim_strip_space_p  drop one leading space; default n, see below
    ensure-cmd curl jq @RET

    local provider="${fim_provider:-codestral}"
    local endpoint="${fim_provider_endpoint[${provider}]}"
    if test -z "${endpoint}" ; then
        ecerr "$0: unknown provider '${provider}'; known: ${(@ok)fim_provider_endpoint}"
        return 1
    fi

    local model="${fim_model:-${fim_provider_model[${provider}]}}"
    local extract="${fim_provider_extract[${provider}]}"
    local max_tokens="${fim_max_tokens:-64}"
    #: `$'\n'' is not expanded inside a parameter default, so it needs its own
    #: variable. Unset means a newline; explicitly empty means no stop at all.
    local newline=$'\n'
    local stop="${fim_stop-${newline}}"
    local temperature="${fim_temperature:-0}"
    local timeout="${fim_timeout:-20}"
    local proxy_p="${fim_proxy_p:-y}"
    local strip_space_p="${fim_strip_space_p:-n}"

    local key_var="${fim_provider_key_var[${provider}]}"
    local api_key="${(P)key_var}"
    if test -n "${key_var}" && test -z "${api_key}" ; then
        #: Better than shipping `Bearer ' and reading back a 401.
        ecerr "$0: no API key for ${provider} (expected \$${key_var})"
        return 1
    fi

    local prefix="${1}"
    local suffix="${2}"
    if test -z "${prefix}${suffix}" ; then
        ecerr "$0: needs a prefix, a suffix, or both"
        return 1
    fi

    #: jq builds the body; the prefix and the suffix are arbitrary buffer text
    #: and must never be interpolated into JSON by hand.
    local req
    req="$(jq --null-input --compact-output \
        --arg model "${model}" \
        --arg prompt "${prefix}" \
        --arg suffix "${suffix}" \
        --arg stop "${stop}" \
        --argjson max_tokens "${max_tokens}" \
        --argjson temperature "${temperature}" \
        '{model: $model, prompt: $prompt, temperature: $temperature}
         + (if $suffix == "" then {} else {suffix: $suffix} end)
         + (if $max_tokens == 0 then {} else {max_tokens: $max_tokens} end)
         + (if $stop == "" then {} else {stop: $stop} end)')" @TRET

    if bool "${proxy_p}" && should-proxy-p ; then
        pxa-local
    fi

    local opts=()
    if isDbg ; then
        ec "${req}" | jq .
    else
        opts+=(--silent)
    fi

    #: `--fail-with-body' is deliberately absent: the status code comes back on
    #: its own last line instead, so both halves of a failure -- the code and
    #: the API's own message -- are available to report.
    local res retcode=0
    res="$(revaldbg curl \
        --location \
        --max-time "${timeout}" \
        --header 'Content-Type: application/json' \
        --header 'Accept: application/json' \
        --header "Authorization: Bearer ${api_key}" \
        --request POST \
        --data "${req}" \
        --write-out $'\n%{http_code}' \
        "${opts[@]}" \
        "${endpoint}")" || retcode=$?

    if (( retcode != 0 )) ; then
        ecerr "$0: ${provider}: curl error ${retcode}"
        return "${retcode}"
    fi

    local http_code="${res##*$'\n'}"
    res="${res%$'\n'*}"
    typeset -g fim_last_res="${res}"

    if (( http_code >= 400 )) ; then
        ecerr "$0: ${provider}: HTTP ${http_code} — $(h-fim-error-message "${res}")"
        return 1
    fi

    local out
    out="$(print -r -- "${res}" | jq --raw-output --join-output "${extract} // empty")" @TRET

    #: Off by default. This used to be unconditional, on the belief that
    #: Codestral had a bug that prepended a stray space. Measured over 29
    #: contexts per provider, that is not what is happening:
    #:
    #:   - All three providers do it at the same rate, so it was never a
    #:     Codestral bug.
    #:   - Where the prefix ends in an operator (`x =', `=>', `|', `a +') the
    #:     space is simply correct, and dropping it gives `count =0'.
    #:   - Where the cursor sits on an empty line, the model supplies the whole
    #:     indent; dropping one space turned eight into seven and broke the
    #:     Python it was completing.
    #:   - Where it really was spurious, it was usually *two* spaces (the model
    #:     re-emitting an indent the prefix already had), which dropping one
    #:     leaves misaligned anyway.
    #:
    #: One case in 87 came out better for it. Left in as a flag rather than
    #: deleted, because a future model may well go back to prepending one.
    if bool "${strip_space_p}" ; then
        out="${out#\ }"
    fi

    if isOutTty ; then
        print -r -- "${out}"
    else
        print -rn -- "${out}"
    fi
}
@opts-setprefix fim-get-v1 fim
##

function h-fim-shell-request {
    local field name value
    print -rn -- "prefix"$'\0'"$1"$'\0'"suffix"$'\0'"$2"$'\0'
    print -rn -- "provider"$'\0'"${fim_provider-}"$'\0'"source"$'\0'"${fim_source:-zsh}"$'\0'
    for field in model max_tokens stop temperature timeout ; do
        name="fim_${field}"
        if (( ${+parameters[$name]} )) ; then
            print -rn -- "$field"$'\0'"${(P)name}"$'\0'
        fi
    done
    for field in strip_space log ; do
        name="fim_${field}_p"
        if (( ${+parameters[$name]} )) ; then
            value=false
            if bool "${(P)name}" ; then value=true ; fi
            print -rn -- "$field"$'\0'"$value"$'\0'
        fi
    done
}

function fim-get {
    #: [agfi:fim-get-v1] is the historical curl rollback, with argv exposure.
    h-fim-providers-refresh || return
    local provider="${fim_provider:-$(command llm_complete fim default-provider)}"
    local key_var="${fim_provider_key_var[$provider]}"
    (
        if [[ -n "$key_var" ]] ; then
            typeset -x "$key_var=${(P)key_var}"
        fi
        if bool "${fim_proxy_p:-y}" && should-proxy-p ; then pxa-local ; fi
        h-fim-shell-request "$1" "$2" | command llm_complete fim --shell
    )
}
@opts-setprefix fim-get fim
