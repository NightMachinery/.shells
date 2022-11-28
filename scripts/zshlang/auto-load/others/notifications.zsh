function notif-os {
    local title="$1" msg="$2"

    if isServer ; then
        return 0
    fi

    if isDarwin ; then
        #: @alt `osascript -e 'display notification "msg" with title "hi"'`
        ##
        if ((${+commands[terminal-notifier]})) ; then
            terminal-notifier -title "$title" -message "$msg"
        else
            ectrace "terminal-notifier not found"
            return 1
        fi
        ##
    else
        @NA
    fi
}

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
