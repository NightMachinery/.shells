#: Google Cloud Translation from the shell.
#:
#: Credentials are private and live in `~/.night-gcp/config.zsh`:
#:   gcp_translate_project  the project billed for the calls
#:   gcp_translate_key_file the service-account JSON (Cloud Translation user role)
#: Tokens are minted by `gcloud` from that key and cached for 50 minutes by
#: default. See `docs/gcp-translate.md`.

typeset -g gcp_translate_project="${gcp_translate_project:-}"
typeset -g gcp_translate_key_file="${gcp_translate_key_file:-}"
typeset -g gcp_translate_source="${gcp_translate_source:-}"  #: empty = detect
typeset -g gcp_translate_token_ttl="${gcp_translate_token_ttl:-3000}"

function gcp-translate-token {
    #: Prints an access token for the translation service account, cached.
    ensure-cmd gcloud @RET
    if [[ -z "${gcp_translate_key_file}" || ! -r "${gcp_translate_key_file}" ]] ; then
        ecerr "$0: set gcp_translate_key_file in ${gcp_conf_file}"
        return 1
    fi
    local cache="${XDG_CACHE_HOME:-${HOME}/.cache}/gcp-translate/token"
    local -a mtime
    if [[ -f "${cache}" && -r "${cache}" && -s "${cache}" ]] &&
        zmodload -F zsh/stat b:zstat 2>/dev/null &&
        zmodload zsh/datetime 2>/dev/null &&
        builtin zstat -A mtime +mtime -- "${cache}" 2>/dev/null &&
        (( EPOCHSECONDS - mtime[1] < gcp_translate_token_ttl )) ; then
        command cat -- "${cache}"
        return
    fi

    ( umask 077; command mkdir -p -- "${cache:h}" ) || return $?
    command chmod 700 "${cache:h}" @RET
    if [[ -d "${cache}" ]] ; then
        ecerr "$0: token cache path is a directory: ${cache}"
        return 1
    fi

    local temporary
    temporary="$(command mktemp "${cache}.XXXXXXXX")" @RET
    {
        GOOGLE_APPLICATION_CREDENTIALS="${gcp_translate_key_file}" \
            command gcloud auth application-default print-access-token > "${temporary}" @RET
        if [[ ! -s "${temporary}" ]] ; then
            ecerr "$0: gcloud returned an empty access token"
            return 1
        fi
        command mv -f -- "${temporary}" "${cache}" @RET
        command cat -- "${cache}"
    } always {
        command rm -f -- "${temporary}"
    }
}

function gcp-translate {
    #: Usage: gcp-translate <target-lang> [text ...]
    #: Text from arguments, else stdin, else the clipboard (via [agfi:in-or-args]).
    #: `gcp_translate_source=de` fixes the source language; empty means detect.
    local target="${1:?target language}" ; shift
    local text
    text="$(in-or-args "$@")" @RET
    [[ -z "${text//[[:space:]]/}" ]] && return 0
    [[ -n "${gcp_translate_project}" ]] || { ecerr "$0: set gcp_translate_project in ${gcp_conf_file}" ; return 1 }
    local token
    token="$(gcp-translate-token)" @RET
    local body
    body="$(jq -n --arg q "$text" --arg t "$target" --arg s "${gcp_translate_source}" \
        '{q: $q, target: $t, format: "text"} + (if $s == "" then {} else {source: $s} end)')" @RET
    command curl -fsS -X POST \
        -H "Authorization: Bearer ${token}" \
        -H "x-goog-user-project: ${gcp_translate_project}" \
        -H 'Content-Type: application/json' \
        'https://translation.googleapis.com/language/translate/v2' \
        --data "$body" \
        | command jq -r '.data.translations[0].translatedText' |
        cat-copy-if-tty
}

function 2en-gcp { gcp-translate en "$@" }
function 2fa-gcp { gcp-translate fa "$@" }
function 2de-gcp { gcp-translate de "$@" }
