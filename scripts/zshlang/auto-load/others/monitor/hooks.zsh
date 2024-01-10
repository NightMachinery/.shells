##
function deluna-hook {
    # @todo1/security Currently implemented via [agfi:deluna], which actually runs it when the computer is unlocked, and also runs it multiple times and when idle etc.
    ##
    killall gpg-agent || true
}

function unlock-hook {
    #: Currently implemented via [agfi:deluna]
    ##
    if false ; then
        # ec $'\n\n'"$0" | sync-append-in "${KARABINER_RESET_LOG}"
        ec $'\n\n'"$0" >> "${KARABINER_RESET_LOG}" @STRUE
        #: I don't want potential lock issues to prevent us from running this.

        karabiner-reset
    fi
}
##
