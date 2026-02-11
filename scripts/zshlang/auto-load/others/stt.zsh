##
function stt-recordings-get {
    fd . ~tmp/hs_whisper | tac
}

function stt-recordings-rerun {
    stt-recordings-get | fz | inargsf rgeval-env with-g25-maybe llm-stt-file
}
##
function llm-focus-p {
    {
        # kitty-tab-codex-p
        ##
        kitty-is-focused && ! kitty-emacs-focused-p
        #: Since knowing when Codex is active behind tmux and SSH is difficult, let us whitelist when we don't want the STT tag instead! It's pretty much only in emacs.
    } ||
        {
            local front_app_cached
            front_app_cached="$(frontapp-get)" @RET

            {
                browser-focus-p &&
                    browser-current-match-p '^https://(chatgpt\.|(?:aistudio|gemini)\.google\.)'
            } ||
                [[ "${front_app_cached}" == 'com.openai.codex' ]]
        }
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
