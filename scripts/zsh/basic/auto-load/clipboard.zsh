teec() {
    doc "teec; ec-and-copy; tee-copy;
Prints and copies its stdin.
See also: 'etee'." #MAGIC
    local out="$(</dev/stdin)"
    <<<"$out" pbcopy
    ec "$out"
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
