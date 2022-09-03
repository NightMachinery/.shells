##
function emc-launch-gui {
    local emacs_dir
    emacs_dir="$(eval-memoi brew --cellar emacs-plus@29)"
    if ! test -d "$emacs_dir" ; then
        if true || isDeus ; then # doing deus will make the common case very slow, as =brew --cellar emacs-plus= keeps returning a non-existent directory
            emacs_dir="$(eval-memoi brew --cellar emacs-plus@29)" # @hardcoded
        else
            reval-ec deus "$0" "$@"
            return $?
        fi
    fi

    local emacs_app
    emacs_app=( "$emacs_dir"/*/Emacs.app/Contents/MacOS/Emacs(N) )
    if (( ${#emacs_app} == 0 )) ; then
        ectrace_ret=127 ectrace "$0: No Emacs.app found; emacs_dir=${emacs_dir}"
        return $?
    elif (( ${#emacs_app} > 1 )) ; then
        ecgray "$0: more than one Emacs.app found"
        emacs_app="${emacs_app[1]}"
    fi

    reval-ec tmuxnewsh2 emacs-gui $proxyenv "${emacs_app}" # --debug-init
}
##
