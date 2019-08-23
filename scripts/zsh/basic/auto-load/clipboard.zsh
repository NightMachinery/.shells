teec() {
    tee /dev/tty | pbcopy
}
etee() {
    mdoc 'etee; Short for `eteec`. An enhancer that copies stdout and prints it, too.
See also: teec' MAGIC
    local out="$(eval "$(gquote "$@")")"
    <<<"$out" pbcopy
    ec "$out"
}
function pbcopy() {
    local in="$(in-or-args "$@")"
    (( $+commands[copyq] )) && {
        silent copyq copy -- "$in"
    } || {
        (( $+commands[pbcopy] )) && command pbcopy <<<"$in"
    }
}
function pbpaste() {
    # local in="$(in-or-args "$@")"
    (( $+commands[copyq] )) && {
        copyq clipboard
    } || {
        (( $+commands[pbpaste] )) && command pbpaste
    }
}
