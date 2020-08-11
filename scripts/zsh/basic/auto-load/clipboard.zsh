function tee-copy() {
    doc "teec; ec-and-copy; tee-copy;
Prints and copies its stdin.
See also: 'etee'."

    > >(pbcopy) | cat
}
aliasfn teec tee-copy
function reval-copy() {
    doc 'revals and also copies the stdout to the clipboard.'

    reval "$@" > >(pbcopy) | cat
}
function ec-copy() {
    reval-copy ec "$@"
}
function pbcopy() {
    local in="$(in-or-args "$@")"
    { false && (( $+commands[copyq] )) } && {
        silent copyq copy -- "$in"
    } || {
        (( $+commands[pbcopy] )) && {
            print -nr -- "$in" | command pbcopy
        }
    }
}
function pbpaste() {
    # local in="$(in-or-args "$@")"
    { false && (( $+commands[copyq] )) } && {
        copyq clipboard
    } || {
        (( $+commands[pbpaste] )) && command pbpaste
    }
}
function pbadd() {
    osascript "$NIGHTDIR"'/applescript/path-copy.applescript' "${(f)$(re realpath $@)}" > /dev/null
}
