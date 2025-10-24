##
function dark-mode-p-darwin {
    local style
    style="$(serr defaults read -g AppleInterfaceStyle)" @RET
    if [[ "${style}" == "Dark" ]]; then
        return 0
    else
        return 1
    fi
}

function dark-mode-p {
    if isRemote ; then
        return 0
        #: @todo We need to contact the client to know the true dark mode status.
    fi

    if isDarwin; then
        dark-mode-p-darwin
    else
        @NA
    fi
}
##
function dark-mode-toggle {
    reval-ec kitty-theme-toggle

    if isDarwin; then
        command dark-mode
    else
        @NA
    fi

    reval-ec fzf-export-opts

    emc-eval '(night/sync-with-system-appearance)'
    withemcgui emc-eval '(night/sync-with-system-appearance)'
}
##
fzf-export-opts
