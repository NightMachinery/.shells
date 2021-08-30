function arger() {
    local i
    for i in "$@" ; do
        ecbold "arg: $i"
    done
}

function argerrainbow() {
    {
        local arg
        for arg in "$@"
        do
            #   fnswap chalk 'command chalk --color 16m' colorfb-r "$arg" | tr -d '\n'
            ecrainbow-n "${arg:-EMPTY_ELEMENT} "
        done
        resetcolor
        echo
    } >&2
}

argerng() {
    clipboard-add-quoted "$@"

    { test -n "$jahmode" || isColor } && ecalternate "$@" || arger "$@"
}

ecdbg() {
    isNotDbg || {
        local errcol=("${debugcol[@]:-cyan}")
        color "$errcol[@]" "$@" >&2
        # errcol=("${debugcol[@]:-cyan}") rederr ecerr "$@"
    }
}

fsaydbg() {
    isNotDbg || {
        ecdbg "$@"
        fsay "$@"
    }
}

dact() {
    doc DEBUG Act
    isNotDbg || eval "$(gquote "$@")" >&2
}
##
function color-override-err {
    if test -z "$isColor_override" ; then
        if isErrTty ; then
            isColor_override=y
        else
            isColor_override=n
        fi
    fi
}
##
function ecnerr-raw() {
    local isColor_override="$isColor_override"
    color-override-err
    ##
    local trace="${ecerr_trace}"
    if bool "$trace" && ! bool "$ectrace_notrace" ; then
        # ectrace uses ecerr when tracing is disabled, so we need to disable notrace or there will be an inf loop
        ectrace_notrace=no ectrace_ret="667" ectrace '' "$*" 1>&2
    else
        ecn "$*" 1>&2
    fi
}

function ecerr-raw() {
    ecnerr-raw "$@"$'\n'
}

function ecerr() {
    local isColor_override="$isColor_override"
    color-override-err
    ##
    ecerr-raw "$(colorfg 255 43 244)$@$(colorreset)"
}

function ecnerr() {
    local isColor_override="$isColor_override"
    color-override-err
    ##
    # we can alsoo use `ecnerr-raw`, but that might break stuff, as ecn is supposed not to add newlines, but the traceback inserts tons of stuff
    ecn "$(colorfg 255 43 244)$@$(colorreset)" 1>&2
}

function ecgray() {
    { colorfg "$gray[@]" ; ec "${@}" ; resetcolor } 1>&2
}
function ecngray() {
    { colorfg "$gray[@]" ; ecn "${@}" ; resetcolor } 1>&2
}
function ecdate() {
    ec "$edPre$(color 100 100 100 $(dateshort))            $@" 1>&2
}
function ecdate-err() {
    ecerr-raw "$(ecdate "$(colorfg 255 50 10)${*}$(resetcolor)")" >&2
}
##
function ecnbold() {
   { Bold ; colorfg 40 200 30 ; Bold ; ecn "$@" ; resetcolor } >&2
}

function ecbold() {
    ecnbold "$*"$'\n'
}
##
function rederr-old() {
    comment Somehow just using pipes sometimes does not work and causes stdout to actually go red ...
    local out="$(mktemp)"
    setopt local_options nomultios pipefail;
    {
        # eval "$(gquote "$@")" 2>&1 1>&3|sed $'s,.*,\e[31m&\e[m,'1>&2
        eval "$(gquote "$@")" 2>&1 1>&3|color "${errcol[@]:-red}" 1>&2
    } 3>"$out"
    cat "$out"
    silent \rm "$out"
}

rederr() {
    comment this is basically raise-blood but for just one command
    exec 3>&2 2> >(color_err)
    reval "$@"
    exec 2>&3 3>&-
}

color_err () {
    ## sysread & syswrite are part of zsh/system
    while sysread std_err_color
    do
        # syswrite -o 2 "${fg_bold[red]}${std_err_color}${terminfo[sgr0]}"
        color "${errcol[@]:-red}" "$std_err_color"
    done
}

dvar () {
    local debugcol
    # eval ecdbg "$1": '$"'"$1"'"'
    local pre=''
    test -z "$2" || pre="CODE $2 | "
    
    debugcol=("$dvar_col1[@]")
    test -z "$dvar_col1" && debugcol=(255 120 0)
    ecdbg "$pre$(typeset -p "$1" 2>&1)"
    debugcol=("$dvar_col2[@]")
    test -z "$dvar_col2" && debugcol=(0 120 255)
    ecdbg "$pre$1 in env: $(printenv "$1")"
}

function raise-blood() ceer rederr.zsh source
##
function retcode() {
    local r=$? ps=("$pipestatus[@]") name="${1:-${$(fn-name 3):-NA}}"

    if (( ${#ps} > 1 )) ; then
        ecerr $'\n'"${name}: returned ${(j.|.)ps[@]}"
    else
        ecerr $'\n'"${name}: returned ${r}"
    fi
    return $r
}
alias e='retcode'
##
function argerdbg() {
    isNotDbg || {
        local errcol=("${debugcol[@]:-cyan}")
        coN=y coNr=y color "$errcol[@]" ''
        argerng "$@"
        resetcolor
    } >&2
}
function revaldbg() {
    ecdbg "$(gq "$@")"
    argerdbg "$@"
    reval "$@"
}
echo-fin() { arger "$fin[@]" } # Useful for debugging env
##
function silencedbg() {
    local cmd=("$@")
    if isDbg
    then
        reval "${cmd[@]}"
    else
        silence "${cmd[@]}"
    fi
}
function sdbg {
    silencedbg "$@"
}

function dbgserr() {
    local cmd=("$@")
    if isDbg
    then
        reval "${cmd[@]}"
    else
        reval serr "${cmd[@]}"
    fi
}
function serrdbg() {
    dbgserr "$@"
}

function dbgsout() {
    local cmd=("$@")
    if isDbg
    then
        reval "${cmd[@]}"
    else
        reval sout "${cmd[@]}"
    fi
}
function soutdbg() {
    dbgsout "$@"
}
##
function ensure() {
    # @deprecated Use `assert` instead
    local caller cmd
    if (( $#@ == 0 )) ; then
        ecerr "$0: called with no arguments."
        return 1
    fi
    if (( $#@ == 1 )) ; then
        cmd="$1"
        caller="$cmd"
    else
        caller="${@[-1]}" cmd=("${@[1,-2]}")
    fi
    ensure_head="$caller" assert "$cmd[@]"
}
function ensure-dbg() {
    # @deprecated Use `assert-dbg` instead
    if isDbg ; then
        ensure "$@"
    else
        reval "${@[1,-2]}"
    fi
}
##
function assert() {
    # Usage: assert true @RET
    # See [[~/cellar/notes/bookmarks/useme/zsh/debugging, stacktraces, exceptions.org]] for tests
    ##
    # `@opts-setprefix assert ensure` is in load-first.zsh
    local msg="${ensure_msg:-$ensure_m}"

    local head
    head="${ensure_head}"

    ## use ectrace_notrace instead
    # local notrace="${ensure_notrace}"
    # if bool "$notrace" ; then
    #     ensure "$@" "$head"
    #     return $?
    # else
    # ...
    ##
    assert_global_lock=y
    # if a nested assert call resets this, we won't print the trace again
    # of course, forked processes can't touch this
    # assert_global_lock is also reset by ectrace (when it actually prints the trace).. We can go and reset it in 'throw' itself ...
    reval "$@" && true
    local ret=$?
    if test -n "$assert_global_lock" && (( ret != 0 ))  ; then
        head="${head:-${$(fn-name):-${funcstack[2]:-assert}}}" # fn-name takes  ~9ms
        test -z "$msg" && msg="$(ecalternate "${@}")" # "(exited $ret)"
        msg="${head}: $msg"

        clipboard-add-quoted "$@"

        ectrace_ret="$ret" ectrace "$msg"
    fi
    # assert_global_lock='' # it's better to trust the one in ectrace, as the trace might have been disabled.
    return $ret
    ## perf tests:
    # `time2 re-val reval assert reval true`
    # `time2 re-val reval reval true`
    # `assert re-val assert false`
    ##
}
function ectrace() {
    {
        ##
        # doesn't work (can't get the pipe)
        # retmsg="$(fnswap isColor false retcode 2>&1 | prefixer --skip-empty)" # making it local loses the retcode :|
        ##
        local ret_orig="${(j.|.)pipestatus[@]}" retmsg
        local ret="${ectrace_ret}" exc="$1" msg="${@[2,-1]}" notrace="${ectrace_notrace}"
        if test -z "$ret" ; then
            retmsg="returned $ret_orig"
            if [[ "$ret_orig" != 0* ]] ; then
                ret=$ret_orig
            else
                ret=666
            fi
        else
            retmsg="exited $ret"
        fi

        if bool "$notrace" ; then
            ecerr "${exc} ($retmsg)"$'\n'
            if test -n "$msg" ; then
                ecerr "$msg"
            fi
        else
            assert_global_lock=''
            throw_ret=$ret throw "$exc" "$msg" | {
                if isColor ; then
                    cat
                else
                    # @todo2 make crash.zsh use our own color infra
                    erase-ansi
                fi
            }
        fi
        return $ret
    } >&2 # @todo make crash.zsh use stderr itself

    ## tests:
    # `return 32 | true || ectrace`
    # `return 32 | true || ectrace_notrace=y ectrace`
    ##
}
function assert-dbg() {
    if isDbg ; then
        assert "$@"
    else
        reval "$@"
    fi
}
##
function assert-args() {
    ensure-args "$@" "${funcstack[2]:-assert}"
}
function ensure-args() {
    # @deprecated Use `assert-args` instead
    if (( $#@ <= 1 )) ; then
        ecerr "$0: not enough arguments."
        return 1
    fi
    local caller="${@[-1]}" args=("${@[1,-2]}")

    local arg ret=0 failed=()
    for arg in "$args[@]" ; do
        if test -z "${(P)arg[*]}" ; then
            ##
            # ecerr "$caller: argument '$arg' is empty."
            ##
            failed+="$arg"
            ##
            ret=1
        fi
    done
    if (( ret != 0 )) ; then
        ectrace_ret="$ret" ectrace "$caller: Empty args: ${(j., .)failed}"
    fi
    return $ret
}
function assert-net() {
    ensure-net "$@" "${funcstack[2]:-assert}"
}
function ensure-net() {
    # @deprecated Use `assert-net` instead
    local ensure_msg="No internet access"
    ensure isNet "$@"
#     if ! isNet ; then
#         ecerr "${1:-$0}: $ensure_msg"
#         return 1
#     fi
}
##
