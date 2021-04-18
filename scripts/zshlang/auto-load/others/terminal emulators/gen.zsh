function terminal-unsupported() {
    ectrace "Unsupported terminal emulator"
    return 1
}
##
function terminal-activate-tab() {
    if isKitty ; then
        kitty-tab-activate "$@"
    elif isiTerm ; then
        iterm-tab-activate "$@"
    else
      terminal-unsupported
    fi
}
function terminal-session-is-focused() {
    if isKitty ; then
        kitty-tab-is-focused "$@"
    elif isiTerm ; then
        iterm-session-is-active "$@"
    else
      terminal-unsupported
    fi
}
function terminal-is-focused() {
    if isKitty ; then
        kitty-is-focused
    elif isiTerm ; then
        iterm-focus-is "$@"
    else
        terminal-unsupported
    fi
}
