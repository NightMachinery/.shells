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
##
function pbcopy-term() {
    # @alt: it2copy
    local in="${$(in-or-args "$@" ; print -n .)[1,-2]}"

    # OSC 52, supported by kitty, iTerm, and others
    printf "\033]52;c;$(printf "%s" "$in" | base64)\a"
}
function pbcopy() {
    # local in="$(in-or-args "$@")"
    local in="${$(in-or-args "$@" ; print -n .)[1,-2]}"

    if isKitty ; then
        ecn "$in" | kitty +kitten clipboard
        return $?
    fi

    { false && (( $+commands[copyq] )) } && {
        silent copyq copy -- "$in"
    } || {
        (( $+commands[pbcopy] )) && {
            print -nr -- "$in" | command pbcopy
        }
    }
}
function pbpaste() {
    if isKitty ; then
        kitty +kitten clipboard --get-clipboard
        return $?
    fi

    { false && (( $+commands[copyq] )) } && {
        copyq clipboard
    } || {
        (( $+commands[pbpaste] )) && command pbpaste
    }
}
function pbadd() {
    osascript "$NIGHTDIR"'/applescript/path-copy.applescript' "${(f)$(re realpath $@)}" > /dev/null
}
function pbpaste-plus() {
    # GLOBAL out: paste
    unset paste
    paste="$(pbpaste)"
    local ppaths=( "${(@f)$(clipboard-to-path.nu)}" )
    test -n "$ppaths[*]" && paste=( $ppaths[@] )
}
