##
aliasfn withemcgui 'EMACS_SOCKET_NAME="$EMACS_GUI_SOCKET_NAME" emacs_night_server_name="$EMACS_GUI_SOCKET_NAME"' reval-env

function emc-launch-gui {
    local env_setters=("${emc_env_setters[@]:-withemcgui}")

    local opts=()

    if isDbg ; then
        opts+='--debug-init'
    fi

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

    retry ensure-redis @RET

    reval-ec tmuxnewsh2 emacs-gui reval-env-ec LOGNAME="$(whoami)" proxy_disabled="$proxy_disabled" DOOMDIR="$DOOMDIR" $proxyenv "${env_setters[@]}" "${emacs_app}" "${opts[@]}"
}
##
