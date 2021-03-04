function notif() {
    local msg="$@"

    ec "NOTIF: $msg"
    ((${+commands[terminal-notifier]})) && terminal-notifier -title "notif" -message "$msg"
    # fsay "Notification. Notification."
    # fsay "$msg"
    tnotif $msg # @FNSWAP
}
function notif-casual() {
    fnswap tnotif tnotif-casual notif "$@"
}
aliasfn notifc notif-casual
##
