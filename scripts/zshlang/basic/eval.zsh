###
alias eval-good='geval'
alias eval-quoted='reval'
alias greval=rgeval

function eval-ec() { ge_no_hist=y geval "$@" }
function eval-ecdbg() { ge_ecdbg=y ge_no_hist=y geval "$@" }
function seval { eval-ecdbg "$@" } #silent debuggable eval
function evaldbg { eval-ecdbg "$@" }

function eval-confirm {
    local cmd="$*"
    if ask "eval?: $cmd" Y ; then
        eval "$cmd"
    fi
}

function reval-confirm {
    local cmd default="${reval_confirm_default:-Y}" before_hook=("${reval_confirm_before[@]}")
    local reval_confirm_default='' reval_confirm_before=''
    cmd=("$@")
    test -z "${cmd[*]}" && return 0

    if ask "reval?: $(ecalternate "${cmd[@]}" 2>&1)" "$default" ; then
        if test -n "${before_hook[*]}" ; then
            reval-ec "${before_hook[@]}" @RET
        fi

        reval "${cmd[@]}"
    else
        return 130 # Control-C is fatal error signal 2, (130 = 128 + 2, see above)
    fi
}

function reval-ec() {
    ## old implementation:
    # ge_no_hist=y rgeval "$@"
    ##
    local ec_engine="${reval_ec_ec_engine:-${reval_ec_e:-ecbold}}" eval_engine="${reval_ec_eval_engine:-eval}"
    local reval_ec_ec_engine='' reval_ec_e='' reval_ec_eval_engine=''
    test -z "$*" && return 0
    local cmd cmd_simple
    cmd="$(gq "$@")" @TRET

    cmd_simple="$(gquote-simple "$@")" @TRET
    "$ec_engine" "$cmd_simple" >&2
    "${eval_engine}" "$cmd"
}
function reval-ecgray { reval_ec_ec_engine=ecgray reval-ec "$@" }

function reval-ecdate() {
   reval_ec_e=ecdate reval-ec "$@"
}

function reval-rainbow() {
    argerng "$@" >&2
    reval "$@"
}
###
function geval() {
    test -z "$*" && return 0
    local cmd="$@"
    test -z "$ge_ecdbg" && {
        test -z "$ge_no_ec"  && { ecbold "$cmd" } >&2
        test -z "$ge_no_hist" && hist-add-unquoted "$cmd" #Add to history
    } || ecdbg "$cmd"
    local ge_ecdbg='' ge_no_ec='' ge_no_hist=''
    eval -- "$cmd"
}
##
function aget() {
    ##
    # "aget does not wait for all forked processed. Probably unsolvable unless we invoke zsh -c"
    # `zsh -f -c 'sleep 3 &'` doesn't wait for them either. We had this problem in borg's old forking system as well.
    ##
    local cmd=("$@")

    local u="./$(uuidgen)" erri jufile j jd
    mkdir -p "$u"
    u="$(realpath "$u")"
    jd="$u"
    ec $u >> "$deleteus"
    test -e "$ag_f" && {
        cp "$ag_f" "$u"/
        ecdbg ag_f: "$ag_f"
    }
    cd "$u"
    test -e "$ag_f" && {
        jufile=(*(.DN))
        j=(${jufile[@]})
    }
    ecdbg jufile: "$jufile"
    if eval "$cmd[@]" ; then
        wait
        test -n "$ag_no_rm" || {
            cd ..
            command rm -r "$u"
        }
    else
        err="$?" && ecerr "aget $cmd[@] exited '$err'. Listing files (can be empty):"
        exa -a -l
        cd ..
        return "$err"
    fi
}
function reval() {
    # ecdbg revaling "$(gquote "$@")"
    # Don't put stuff here, reval is used in ecdbg itself!
    local cmd="$(gquote "$@")"
    test -z "$*" && return 0 # Don't throw an error, it throws some other stuff up :|
    eval "$cmd"
}

function reval-true() {
    reval "$@" || true
}

function reval-not {
    ! reval "$@"
}
alias not='reval-not'

function reval-withstdin {
    ecn "$1" | reval "${@[2,-1]}"
}
alias rin='reval-withstdin'

function reval-to-stdout {
    reval "$@" 2>&1
}

function rgeval() {
    geval "$(gquote "$@")"
}
##
function cat-postprocess {
    #: * @tests
    #: ** `arrnn {1..10} | cat-postprocess 'prefixer -a hi' 'cat -vte'`
    ##
    local postprocessors=($@)

    if (( ${#postprocessors} == 0 )) ; then
        cat
    else
        "${=postprocessors[1]}" | cat-postprocess "${(@)postprocessors[2,-1]}"
    fi
}
##
