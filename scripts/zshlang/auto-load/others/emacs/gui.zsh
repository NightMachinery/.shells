##
aliasfn withemcgui 'EMACS_SOCKET_NAME="$EMACS_GUI_SOCKET_NAME" emacs_night_server_name="$EMACS_GUI_SOCKET_NAME"' reval-env

function emc-launch-gui {
    local env_setters=("${emc_env_setters[@]:-withemcgui}")

    local opts=()

    if isDbg ; then
        opts+='--debug-init'
    fi

    local emacs_app="${NIGHT_EMACS_APP:-}"
    if [[ -z "$emacs_app" ]] ; then
        local emacs_dir
        emacs_dir="$(eval-memoi brew --cellar emacs-plus@29 2>/dev/null)"
        local candidates=()
        #: An absent formula returns no path. Globbing it would search /* and
        #: can hang on an automounted network directory during a GUI launch.
        if [[ -n "$emacs_dir" && -d "$emacs_dir" ]] ; then
            candidates=( "$emacs_dir"/*/Emacs.app/Contents/MacOS/Emacs(N) )
        fi
        if (( ${#candidates} > 0 )) ; then
            emacs_app="${candidates[1]}"
        elif [[ -x /Applications/Emacs.app/Contents/MacOS/Emacs ]] ; then
            emacs_app=/Applications/Emacs.app/Contents/MacOS/Emacs
        fi
    fi
    if [[ -z "$emacs_app" || ! -x "$emacs_app" ]] ; then
        ectrace_ret=127 ectrace "$0: No executable Emacs.app found; set NIGHT_EMACS_APP to its executable"
        return $?
    fi

    retry ensure-redis @RET

    # CC="gcc-15" PATH="$PATH"
    reval-ec tmuxnewsh2 emacs-gui reval-env-ec LOGNAME="$(whoami)" proxy_disabled="$proxy_disabled" DOOMDIR="$DOOMDIR" $proxyenv "${env_setters[@]}" "${emacs_app}" "${opts[@]}"
}
##
