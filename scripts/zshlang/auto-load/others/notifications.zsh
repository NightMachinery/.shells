function notif-app-install {
    : "builds dedicated macOS notification apps: notif-app-install [agents|mail] [--check]"
    local profile="${1:-agents}" night_dir="${NIGHTDIR:-${HOME}/scripts}"
    (( $# )) && shift
    ensure-cmd python3 @RET
    command python3 "${night_dir}/macos/notification-apps/build.py" "$profile" "$@"
}

function h-notif-os-command {
    #: Both posting and removal must use the same signed app identity.
    local app="${notif_app:-}" app_file
    case "$app" in
        agents) app_file="${HOME}/Applications/Agent Bells.app/Contents/MacOS/terminal-notifier" ;;
        mail) app_file="${HOME}/Applications/Night Mail.app/Contents/MacOS/terminal-notifier" ;;
        '') ensure-cmd terminal-notifier @RET; REPLY="${commands[terminal-notifier]}"; return 0 ;;
        *) ecerr "$0: unknown notif_app: $app"; return 1 ;;
    esac
    if [[ ! -x "$app_file" ]] ; then
        [[ "$1" == --no-install ]] && return 1
        #: No fallback to the generic app: that would break dismissal isolation.
        notif-app-install "$app" || return $?
    fi
    REPLY="$app_file"
}

function notif-os-dismiss-agent {
    : "dismisses desktop agent-bell notifications only; preserves mail and other apps"
    notif_app=agents notif-os-remove ALL
}

function notif-os-dismiss-all {
    #: @duplicateCode/b88a767cde3e47cb1bad4b855a8989b8: hyper+d calls the
    #: same notifDismissAll, in hammerspoon/core/notif-dismiss.lua. It is far
    #: faster than the JXA script, which stays as the fallback for when
    #: Hammerspoon is not answering.
    if isDarwin ; then
        ##
        if [[ "$(hammerspoon_assert_p=n hammerspoon -c 'notifDismissAll() return "ok"' 2>/dev/null)" != ok ]] ; then
            notif-dismiss-v2.jxa
        fi
        ##
        # notif-dismiss.as
        ##
    else
        @NA
    fi
}

function notif-os-remove {
    : "removes the undismissed desktop notification posted with notif_group=<group>"
    #: The counterpart of the `-group' option [agfi:notif-os] passes: a source that
    #: restates one fact (an agent waiting) can take the notification back once the
    #: fact stops holding, instead of leaving it for the user to close by hand.
    ##
    local group="${1}"
    test -n "$group" || return 1

    if isServer ; then
        return 0
    fi

    if isDarwin ; then
        local REPLY notifier
        h-notif-os-command --no-install || return 0
        notifier="$REPLY"
        reval-timeout 12 "$notifier" -remove "$group" >/dev/null
    else
        @NA
    fi
}

function notif-os {
    local title="$1" msg="$2"
    local subtitle="${notif_subtitle:-}"
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
        local REPLY notifier
        if h-notif-os-command ; then
            notifier="$REPLY"
            local opts=()
            test -n "$subtitle" && opts+=(-subtitle "\\${subtitle}")
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

            #: NSUserDefaults parses option values as plists. A leading backslash
            #: forces literal text and terminal-notifier strips that backslash.
            reval-timeout 12 "$notifier" -title "\\${title}" -message "\\${msg}" "$opts[@]"
        else
            ectrace "notification app is unavailable"
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
    local msg="$@" tlg="${notif_tlg}" title="${notif_title:-}"
    local desktop_msg="${notif_body-$msg}"

    ec "NOTIF: $msg"
    notif-os "$title" "$desktop_msg"

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
