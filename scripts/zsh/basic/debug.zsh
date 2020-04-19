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
ecerr() ec "$@" 1>&2
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
	}
}
function evaldbg() {
	ecdbg "$@"
	argerdbg "$@"
	reval "$@"
}
echo-fin() { arger "$fin[@]" } # Useful for debugging env
