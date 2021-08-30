## @todo0 these should be in core.zsh
function isdefined() {
    local sym="$1"

    test -n "$sym" && (( $+commands[$sym] || $+functions[$sym] || $+aliases[$sym] ))
}
alias isDefined=isdefined

function ifdefined() {
    local cmd_head="$1"

    if isdefined "$cmd_head" ; then
        reval "$@"
    else
        return 1270 # == 246
    fi
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
    assert-args dep install_cmd || return $?

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
