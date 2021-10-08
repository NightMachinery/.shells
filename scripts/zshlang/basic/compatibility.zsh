## @todo0 these should be in core.zsh
function isdefined() {
    local sym="$1"

    test -n "$sym" && (( $+commands[$sym] || $+functions[$sym] || $+aliases[$sym] ))
}
alias isDefined=isdefined

function ifdefined() {
    # @duplicateCode/2835fdc8e7eb4fedc98965e17db301b6
    ##
    local cmd_head="$1"

    if isdefined "$cmd_head" ; then
        reval "$@"
    else
        return 1270 # == 246
    fi
}

function isdefined-cmd {
     local sym="$1"

     test -n "$sym" && (( $+commands[$sym] ))
}

function ifdefined-cmd() {
    # @duplicateCode/2835fdc8e7eb4fedc98965e17db301b6
    ##
    local cmd_head="$1"

    if isdefined-cmd "$cmd_head" ; then
        reval "$@"
    else
        return 1270 # == 246
    fi
}
alias 'isDefined-cmd'=isdefined-cmd

function cmd-sub() {
    local cmd="$1" sub="$2"

    if isdefined-cmd "$cmd" ; then
        print -nr -- "$cmd"
    else
        print -nr -- "$sub"
    fi
}

function ifdefined-cmd-or-cat {
    local cmd="$1" ; shift
    assert-args cmd @RET

    if isdefined-cmd "$cmd" ; then
        "$cmd" "$@"
    else
        ecerr "$(fn-name 3): $(gquote-sq $cmd) not found, falling back to 'cat'"

        cat
    fi

    ## @tests
    # `aliasfn t-cat ifdefined-cmd-or-cat nonexistent-918982`
    # `aliasfn t-char-count ifdefined-cmd-or-cat wc -c`
    ##
}
##
function ensure-dep1 {
    local dep="$1" install_cmd=("${@[2,-1]}")
    assert-args dep install_cmd || return $?

    if ! isdefined-cmd "${dep}" ; then
        assert reval "$install_cmd[@]"

        rehash
        if ! isdefined-cmd "${dep}" ; then
            ecerr "$0: could not install $(gquote-sq "$dep")"
            return 1
        fi
    fi
}
##
