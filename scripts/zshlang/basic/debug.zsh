arger() { re 'ec arg:' "$@" }
argerrainbow() {
	local arg
	for arg in "$@"
	do
	#	fnswap chalk 'command chalk --color 16m' colorfb-r "$arg" | tr -d '\n'
		ecrainbow-n "${arg:-EMPTY_ELEMENT} "
	done
	resetcolor
	echo
}
argerng() {
	{ test -n "$jahmode" || isI } && ecalternate "$@" || arger "$@"
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
    isNotDbg || eval "$(gquote "$@")"
}
##
function ecnerr-raw() {
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
    ecerr-raw "$(colorfg 255 43 244)$@$(colorreset)"
}
function ecnerr() {
    # we can alsoo use `ecnerr-raw`, but that might break stuff, as ecn is supposed not to add newlines, but the traceback inserts tons of stuff
    ecn "$(colorfg 255 43 244)$@$(colorreset)" 1>&2
}
function ecdate() {
    ec "$edPre$(color 100 100 100 $(dateshort))            $@"
}
function ecdate-err() {
    ecerr-raw "$(ecdate "$(colorfg 255 50 10)${*}$(resetcolor)")" >&2
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
function retcode() {
    local r=$? ps=("$pipestatus[@]")

    if (( ${#ps} > 1 )) ; then
        ecerr $'\n'Returned ${r}: "${(j.|.)ps[@]}"
    else
        ecerr $'\n'Returned ${r}
    fi
    return $r
}
alias e='retcode'
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
function dbgserr() {
    local cmd=("$@")
    if isDbg
    then
        reval "${cmd[@]}"
    else
        reval serr "${cmd[@]}"
    fi
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
    head="${ensure_head:-${$(fn-name):-assert}}"

    ## use ectrace_notrace instead
    # local notrace="${ensure_notrace}"
    # if bool "$notrace" ; then
    #     ensure "$@" "$head"
    #     return $?
    # else
    # ...
    ##
    reval "$@"
    local ret=$?
    test -z "$msg" && msg="$(ecalternate "${@}")" # "(exited $ret)"
    msg="${head}: $msg"
    if (( ret != 0 ))  ; then
        ectrace_ret="$ret" ectrace "$msg"
    fi
    return $ret
}
function ectrace() {
    {
        local ret="${ectrace_ret:-666}" exc="$1" msg="${@[2,-1]}" notrace="${ectrace_notrace}"

        if bool "$notrace" ; then
            ecerr "${exc} (exited $ret)"$'\n'
            if test -n "$msg" ; then
                ecerr "$msg"
            fi
        else
            throw_ret=$ret throw "$exc" "$msg" | {
                if isI ; then
                    cat
                else
                    # @todo2 make crash.zsh use our own color infra
                    erase-ansi
                fi
            }
        fi
        return $ret
    } >&2 # @todo make crash.zsh use stderr itself
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
        if test -z "${(P)arg}" ; then
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
