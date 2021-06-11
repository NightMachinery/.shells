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
function ensure-dep1 {
    local dep="$1" install_cmd=("${@[2,-1]}")
    assert-args dep install_cmd @RET

    if test -z "${commands[$dep]}" ; then
        assert reval "$install_cmd[@]"

        rehash
        if test -z "${commands[$dep]}" ; then
            ecerr "$0: could not install $(gquote-sq "$dep")"
            return 1
        fi
    fi
}
##
