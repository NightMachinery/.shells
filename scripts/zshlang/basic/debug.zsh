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
ecerr() { color 255 43 244 "$@" 1>&2 }
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
function e() {
    echo $? "${pipestatus[@]}" "${PIPESTATUS[@]}"
}
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
    local msg="${ensure_msg:-$ensure_m}"
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
    reval "$cmd[@]" && return 0
    local ret=$?
    test -z "$msg" && msg="$(ecalternate "$cmd[@]") (exited $ret)"
    ##
    ecerr "$caller: $msg"
    # ecerr "$caller: Failed $ret: $(gq "$cmd[@]")"
    # ecerr "$caller: Failed $ret"$'\n    '"$(gq "$cmd[@]")"
    ##
    return $ret
}
function ensure-args() {
    if (( $#@ <= 1 )) ; then
        ecerr "$0: not enough arguments."
        return 1
    fi
    local caller="${@[-1]}" args=("${@[1,-2]}")

    local arg ret=0
    for arg in "$args[@]" ; do
        if test -z "${(P)arg}" ; then
            ecerr "$caller: argument '$arg' is empty."
            ret=1
        fi
    done
    return $ret
}
function ensure-net() {
    local ensure_msg="No internet access"
    ensure isNet "$@"
#     if ! isNet ; then
#         ecerr "${1:-$0}: $ensure_msg"
#         return 1
#     fi
}
##
