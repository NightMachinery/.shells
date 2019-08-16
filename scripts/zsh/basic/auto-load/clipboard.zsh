function pbcopy() {
    local in="$(in-or-args "$@")"
    (( $+commands[copyq] )) && False && {
        silent copyq copy "$in"
    } || {
        (( $+commands[pbcopy] )) && command pbcopy <<<"$in"
    }
}
function pbpaste() {
    # local in="$(in-or-args "$@")"
    (( $+commands[copyq] )) && False && {
        copyq clipboard
    } || {
        (( $+commands[pbpaste] )) && command pbpaste
    }
}
