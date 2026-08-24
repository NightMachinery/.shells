function notif-os-dismiss-all {
    if isDarwin ; then
        ##
        notif-dismiss-v2.jxa
        ##
        # notif-dismiss.as
        ##
    else
        @NA
    fi
}

function notif-os {
    local title="$1" msg="$2"
    #: On by default: a notification raised by our own code is almost always worth
    #: seeing, whereas Do Not Disturb is aimed at calls and social apps. Override per
    #: call with `notif_ignore_dnd_p=n`.
    local ignore_dnd="${notif_ignore_dnd_p:-y}"
    #: Path to an image shown inside the notification body. See [agfi:app-icon-get].
    local image="${notif_image}"
    #: Notifications sharing a group replace each other: posting removes the
    #: previous undismissed notification of the same group first. For sources
    #: that restate one fact (an agent still waiting), set this to a stable key
    #: so repeats update the existing notification instead of piling up.
    #: Empty (the default) posts an ordinary ungrouped notification.
    local group="${notif_group}"

    if isServer ; then
        return 0
    fi

    if isDarwin ; then
        #: @alt `osascript -e 'display notification "msg" with title "hi"'`
        ##
        if ((${+commands[terminal-notifier]})) ; then
            local opts=()
            if bool "$ignore_dnd" ; then
                opts+=(-ignoreDnD)
            fi

            if test -n "$group" ; then
                opts+=(-group "$group")
            fi

            if test -n "$image" ; then
                #: -contentImage, not -appIcon: macOS takes the icon from the sending
                #: bundle and silently ignores -appIcon, so the only way to show our
                #: own artwork is the image in the notification body.
                #: @warn terminal-notifier exits 0 for a missing image, so check first.
                if test -e "$image" ; then
                    opts+=(-contentImage "$image")
                else
                    ecgray "$0: notif_image does not exist: ${image}"
                fi
            fi

            terminal-notifier -title "$title" -message "$msg" "$opts[@]"
        else
            ectrace "terminal-notifier not found"
            return 1
        fi
        ##
    else
        @NA
    fi
}
aliasfn os-notif notif-os

function notif-kitty {
    local title="$1" msg="$2"

    if isKitty ; then
        printf '\x1b]99;i=1:d=0;%s\x1b\\\x1b]99;i=1:d=1:p=body;%s\x1b\\' "$title" "$msg" >/dev/tty
    fi
}
##
function notif {
    local msg="$@" tlg="${notif_tlg}"

    ec "NOTIF: $msg"
    notif-os "" "$msg"

    # fsay "Notification. Notification."
    # fsay "$msg"

    if bool "$tlg" || { test -z "$tlg" && isServer } ; then
        tnotif $msg # @FNSWAP
    fi
}
function notif-casual() {
    notif_tlg="${notif_tlg:-y}" fnswap tnotif tnotif-casual notif "$@"
}
aliasfn notifc notif-casual
##
