##
function stt-recordings-get {
    fd . ~tmp/hs_whisper | tac
}

function stt-recordings-rerun {
    stt-recordings-get | fz | inargsf rgeval-env with-argon-maybe llm-stt-file
}
##
#: Add native apps by bundle ID and web apps by URL regex here.
typeset -ga llm_focus_app_ids=(
    'com.openai.codex'
    'com.anthropic.claudefordesktop'
    'sh.paseo.desktop'
    'sh.paseo.ypaseo.desktop'
)
typeset -ga llm_focus_url_patterns=(
    '^https://chatgpt\.'
    '^https://(?:aistudio|gemini)\.google\.'
    '^https://claude\.'
)

function llm-focus-p {
    : "Return true when dictated speech should get the speech-to-text tag."
    #: @keywords stt-p
    local -a app_ids=("${llm_focus_app_ids[@]}")
    local -a url_patterns=("${llm_focus_url_patterns[@]}")

    #: Kitty cannot reliably identify LLM sessions behind tmux or SSH, so tag
    #: terminal dictation except when Emacs is focused.
    kitty-is-focused && ! kitty-emacs-focused-p && return 0

    local front_app_cached app_id
    front_app_cached="$(frontapp-get)" @RET
    for app_id in "${app_ids[@]}" ; do
        [[ "${front_app_cached}" == "${app_id}" ]] && return 0
    done

    #: Join the patterns so the browser URL is fetched only once. An empty
    #: list disables web matching instead of matching every URL.
    (( ${#url_patterns} )) || return 1
    browser-focus-p && browser-current-match-p "${(j:|:)url_patterns}"
}

function h-stt-filter {
    local f="$1"

    local end=''
    if llm-focus-p ; then
        ##
        # snippet-stt-coding ''
        ##
        # ec 'The following text has been dictated using STT.'
        ec '```speech-to-text'
        end=$'\n''```'$'\n'
    fi

    cat "${f}"
    ecn "${end}"
}
##
