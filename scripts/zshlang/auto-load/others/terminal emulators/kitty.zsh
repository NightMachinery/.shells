function kitty-remote() {
    : "Invoke with no args to enter the kitty shell"

    if isKitty ; then
        kitty @ "$@"
    else
        local s i
        s=("$HOME/tmp/.kitty"*(DN)) # do NOT use '.' glob as these aren't 'files'
        if (( $#s >= 1 )) ; then
            # dead kitties can leave trash sockets behind
            for i in ${s[@]} ; do
                kitty @ --to unix:${i} "$@"
            done
        fi
    fi
}
function kitty-send() {
    local m="${kitty_send_match}"
    in-or-args2 "$@"

    ec "${inargs[@]}" | kitty-remote send-text --match "$m" --stdin
    # https://sw.kovidgoyal.net/kitty/remote-control.html#kitty-send-text
}
function kitty-C-c() {
    kitty-send $'\C-c' #$'\n''reset'
}
@opts-setprefixas kitty-C-c kitty-send
function kitty-esc() {
    local m="${kitty_send_match}"
    # kitty-send $'\^['
    # kitty-send "$(printf '\x1b')"
    printf -- '\x1b\x1b\x1b\x1b\x1b\n' | kitty-remote send-text --match "$m" --stdin
}
@opts-setprefixas kitty-esc kitty-send
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
