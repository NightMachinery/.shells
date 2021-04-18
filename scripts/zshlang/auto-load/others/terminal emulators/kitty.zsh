function kitty-remote() {
    : "Invoke with no args to enter the kitty shell"

    kitty @ --to unix:"$HOME/tmp/.kitty" "$@"
}
function kitty-send() {
    in-or-args2 "$@"

    ec "${inargs[@]}" | kitty-remote send-text --stdin
    # https://sw.kovidgoyal.net/kitty/remote-control.html#kitty-send-text
}
function kitty-C-c() {
    kitty-send $'\C-c' #$'\n''reset'
}
function kitty-esc() {
    # kitty-send $'\^['
    # kitty-send "$(printf '\x1b')"
    printf -- '\x1b\x1b\x1b\x1b\x1b\n' | kitty-remote send-text --stdin
}
##
function kitty-tab-activate() {
    local i="${1:-5}"

    kitty-remote focus-tab --match="id:${i}"
    # https://sw.kovidgoyal.net/kitty/remote-control.html#kitty-focus-tab"
}
##
function kitty-tab-get() {
    local id="${1:-$KITTY_WINDOW_ID}"
    assert-args id @RET

    kitty-remote ls | jq -r --arg wid "$id" '.[] | select(.is_focused == true) | .tabs[] | select(.id == ($wid | tonumber))'
}
function kitty-tab-get-emacs() {
    kitty-remote ls | jq -r --arg wid "$id" '.[] | select(.is_focused == true) | .tabs[] | select(.windows[0].foreground_processes[0].cmdline[0] | contains("emacs") ) | .id'
}
function kitty-emacs-focus() {
    local id
    id="$(kitty-tab-get-emacs)" @TRET
    kitty-tab-activate "$id"
}

function kitty-tab-is-focused() {
    [[ "$(kitty-tab-get | jq -r '.is_focused')" == 'true' ]]
}
##
redis-defvar kitty_focused
function kitty-is-focused() {
    ## does not work, is always true, probably tracks the last active kitty window regardless of OS focus:
    # kitty-remote ls | jq -r '.[] | .is_focused'
    ##
    [[ "$(kitty_focused_get)" == 1 ]]
}
##
