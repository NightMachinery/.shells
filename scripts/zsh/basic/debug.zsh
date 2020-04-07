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
