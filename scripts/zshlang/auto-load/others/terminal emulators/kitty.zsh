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
    kitty-send $'\^['
}
