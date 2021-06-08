## @todo0 these should be in core.zsh
function isdefined() {
    local sym="$1"

    test -n "$sym" && (( $+commands[$sym] || $+functions[$sym] || $+aliases[$sym] ))
}
function cmd-sub() {
    local cmd="$1" sub="$2"

    if isdefined "$cmd" ; then
        print -nr -- "$cmd"
    else
        print -nr -- "$sub"
    fi
}
##
