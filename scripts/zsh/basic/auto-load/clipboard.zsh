function pbcopy() {
    local in="$(in-or-args "$@")"
    (( $+commands[copyq] )) && false && {
        silent copyq copy "$in"
    } || {
        (( $+commands[pbcopy] )) && command pbcopy <<<"$in"
    }
}
function pbpaste() {
    # local in="$(in-or-args "$@")"
    (( $+commands[copyq] )) && false && {
        copyq clipboard
    } || {
        (( $+commands[pbpaste] )) && command pbpaste
    }
}
