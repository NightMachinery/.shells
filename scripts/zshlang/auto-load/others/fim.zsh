##
#: Fill-in-the-middle (FIM) completion: hand a model the text before the
#: cursor and the text after it, get back the missing middle.
#:
#: This file is the composable half -- [agfi:fim-get] works in any shell, in a
#: pipe, or over brish. The interactive half, an =alt+.= widget, lives in
#: =zshlang/interactive/auto-load/FIM.zsh= and is only loaded in interactive
#: shells.
#:
#: The Emacs twin is =night/fim-get= in
#: =~/doom.d/autoload/night-mistral-fim.el=. Both speak the same request body
#: to the same three providers, so a change to one wants the same change to
#: the other.
#:
#: See =docs/fim.md=.
##
#: Every native FIM API takes an identical body -- model, prompt, suffix,
#: max_tokens, stop, temperature -- so a provider is just four strings.
##
typeset -gA fim_provider_endpoint=(
    codestral      "https://codestral.mistral.ai/v1/fim/completions"
    deepseek       "https://api.deepseek.com/beta/completions"
    deepseek-flash "https://api.deepseek.com/beta/completions"
)

typeset -gA fim_provider_model=(
    codestral      codestral-latest
    deepseek       deepseek-v4-pro
    deepseek-flash deepseek-v4-flash
)

#: The name of the global holding the key, not the key itself, so that no key
#: material is ever an argument and `ps' cannot see it.
typeset -gA fim_provider_key_var=(
    codestral      codestral_api_key
    deepseek       deepseek_api_key
    deepseek-flash deepseek_api_key
)

#: Mistral answers at .choices[0].message.content; DeepSeek's /beta endpoint is
#: OpenAI-shaped and answers at .choices[0].text.
typeset -gA fim_provider_extract=(
    codestral      ".choices[0].message.content"
    deepseek       ".choices[0].text"
    deepseek-flash ".choices[0].text"
)

typeset -g fim_provider="${fim_provider:-codestral}"
##
function fim-providers {
    #: Names usable as `fim_provider'.
    print -rl -- "${(@ok)fim_provider_endpoint}"
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
function fim-get {
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
    #:   fim_strip_space_p  default y; see below
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
    local strip_space_p="${fim_strip_space_p:-y}"

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

    #: Codestral is buggy and often prepends a space. Dropping it can
    #: occasionally be wrong too, but much less often than keeping it.
    if bool "${strip_space_p}" ; then
        out="${out#\ }"
    fi

    if isOutTty ; then
        print -r -- "${out}"
    else
        print -rn -- "${out}"
    fi
}
@opts-setprefix fim-get fim
##
