function notif() {
    local msg="$@"

    ec "NOTIF: $msg"
    terminal-notifier -title "notif" -message "$msg"
    # fsay "Notification. Notification."
    # fsay "$msg"
    tsm $msg
}
