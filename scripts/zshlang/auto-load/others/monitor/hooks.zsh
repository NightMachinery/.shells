##
function lock-security {
    killall gpg-agent || true
}
##
function h-hook-deluna {
    # Currently implemented via [agfi:deluna], which actually runs it when the computer is unlocked, and also runs it multiple times and when idle etc.
    ##
    lock-security
}
##
function h-hook-lock {
    #: [[id:597345ed-90e6-491b-9c70-43db87bc707d][@good lock_watcher.swift]]
    ##
    h-lunaquit-force-skip-reset

    lock-security
}

function h-hook-unlock {
    #: [[id:597345ed-90e6-491b-9c70-43db87bc707d][@good lock_watcher.swift]]
    #: @old [agfi:deluna]
    ##
    lock-security

    reval-ecdate last-idle-reset
    reval-ecdate luna-skip-reset

    if false ; then
        # ec $'\n\n'"$0" | sync-append-in "${KARABINER_RESET_LOG}"
        ec $'\n\n'"$0" >> "${KARABINER_RESET_LOG}" @STRUE
        #: I don't want potential lock issues to prevent us from running this.

        karabiner-reset
    fi

    battery-charge-limit-restore-status
    #: If the laptop's battery dies and we turn it on again, our restart hooks won't run but the limit would be reset.
}
##
function tealy-connect-hook {
    # fsay "Tealy connected"

    darwin-net-static-set
    sleep 5
    darwin-net-static-set
}

function wifi-disconnect-hook {
    darwin-net-static-unset
    sleep 5
    darwin-net-static-unset
}

function wifi-unknown-connect-hook {
    darwin-net-static-unset
    sleep 5
    darwin-net-static-unset
}
##
