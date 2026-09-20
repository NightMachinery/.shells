#: Google Cloud Translation from the shell.
#:
#: Credentials are private and live in `~/.night-gcp/config.zsh`:
#:   gcp_translate_project  the project billed for the calls
#:   gcp_translate_key_file the service-account JSON (Cloud Translation user role)
#: Tokens are minted by `gcloud` from that key and cached for an hour. Nothing
#: identifying lives in this file.

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
    local cache="${XDG_CACHE_HOME:-$HOME/.cache}/gcp-translate/token"
    if [[ -r "$cache" ]] && (( $(date +%s) - $(stat -f %m "$cache" 2>/dev/null || stat -c %Y "$cache") < gcp_translate_token_ttl )) ; then
        cat "$cache"
        return 0
    fi
    mkdir -p "${cache:h}" && chmod 700 "${cache:h}"
    GOOGLE_APPLICATION_CREDENTIALS="${gcp_translate_key_file}" \
        gcloud auth application-default print-access-token > "$cache" @RET
    chmod 600 "$cache"
    cat "$cache"
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
        | jq -r '.data.translations[0].translatedText'
}

function 2en-gcp { gcp-translate en "$@" }
function 2fa-gcp { gcp-translate fa "$@" }
function 2de-gcp { gcp-translate de "$@" }
