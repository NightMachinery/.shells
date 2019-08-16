arger() { re 'ec arg:' "$@" }
ecdbg() {
    test -z "$DEBUGME" || {
        errcol=("${debugcol[@]:-cyan}") rederr ecerr "$@"
    }
}
fsaydbg() {
    test -z "$DEBUGME" || {
        ecdbg "$@"
        fsay "$@"
    }
}
dact() {
    doc DEBUG Act
    test -z "$DEBUGME" || eval "$(gquote "$@")"
}
ecerr() ec "$@" 1>&2
function rederr() {
	  (setopt nomultios 2>/dev/null; set -o pipefail;
     # eval "$(gquote "$@")" 2>&1 1>&3|sed $'s,.*,\e[31m&\e[m,'1>&2
     eval "$(gquote "$@")" 2>&1 1>&3|color "${errcol[@]:-red}" 1>&2
    )3>&1
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
