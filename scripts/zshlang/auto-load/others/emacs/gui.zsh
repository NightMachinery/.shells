##
function emc-launch-gui {
    local emacs_app
    emacs_app=( "$(brew --cellar emacs-plus )/"*/Emacs.app/Contents/MacOS/Emacs(N) )
    if (( ${#emacs_app} == 0 )) ; then
        ectrace_ret=127 ectrace "$0: No Emacs.app found"
        return $?
    elif (( ${#emacs_app} > 1 )) ; then
        ecgray "$0: more than one Emacs.app found"
        emacs_app="${emacs_app[1]}"
    fi

    tmuxnewsh2 emacs-gui $proxyenv "${emacs_app}"
}
##
