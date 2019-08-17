## Alias Module
function expand-alias {
    doc This only expands once. To expand all aliases, use 'expand-aliases'.
    if [[ -n $ZSH_VERSION ]]; then
        # shellcheck disable=2154  # aliases referenced but not assigned
        printf '%s\n' "${aliases[$1]}"
    else  # bash
        printf '%s\n' "${BASH_ALIASES[$1]}"
    fi
}
function force-expand {
    local e="$(expand-aliases "$1")"
    test -z "$e" && e="$1"
    echo "$e"
}
isMalice() { ! isSSH && [[ -z "$disable_malice" ]] }
isNotMalice() { ! isMalice }
expand-aliases() {
    doc 'See https://unix.stackexchange.com/questions/372811/better-understand-a-function-expanding-aliases-in-zsh'
    unset 'functions[_expand-aliases]'
    functions[_expand-aliases]="$@"
    (($+functions[_expand-aliases])) && ec "${functions[_expand-aliases]#$'\t'}"
}
# Forked from https://blog.sebastian-daschner.com/entries/zsh-aliases
alias-special() {
    local args=("$@[2,-1]")
    # re 'ec arg:' "$args[@]"
    # ec "$args[*]"
    builtin alias "$args[@]"
    unset match
    isNotMalice || [[ -z "$args[*]" ]] || [[ "$args[1]" == -* ]] || { [[ "$args[*]" =~ '\s*([^=]*)=([^\s]*)\s?.*' ]] &&
        {
		[[ "$args[*]" =~ '\s*([^=]*)=([^\s]*)\s?.*' ]]
	    # there is a bug in either ssh or ubuntu's zsh that loses the match variable here so I just rematch it
		# dbg dvar match
            # test -z "$DEBUGME" || { mhat3=("$match[@]") ; mhat2=("$args[@]") }
            test -z "$match[1]" && { ecerr Empty alias detected. Called with "$args[@]" ; return 1 }
            unset "ialiases[${(b)match[1]}]"
            unset "baliases[${(b)match[1]}]"
            eval "$1[\$match[1]]=y"
            # doc we do not expand recursive aliases because that changes behavior
            # doc for an example, try 'alias arger="arger a b"' with this safety disabled
            # doc we can add a new class of once-exepanders that only expand once
            # doc we could also ditch the automatic full expansion and manually expand till safety
            # doc note that if the recursive alias is defined after this one, we will fail to detect it with our current brittle scheme.
            ! { [[ "$match[2]" == "$match[1]" ]] || (( $+ialiases[$match[2]] )) } || {
                # test -z "$DEBUGME" || print -r ialiasing "$match[1]" to avoid recursion
                ialiases[$match[1]]=y
            }
        } || { ecerr aliasing "$args[*]" failed ; test -z "$DEBUGME" || mhat=("$args[@]") } }
}
typeset -Ag baliases
typeset -Ag ialiases
typeset -Ag naliases #normal aliases #now useless

balias() as_caller=$0 alias-special baliases "$@"
ialias() as_caller=$0 alias-special ialiases "$@"
alias() as_caller=$0 alias-special naliases "$@"

expand-alias-space() {
    (( $+baliases[$LBUFFER] )) ; insertBlank=$?
    [[ -n "$RBUFFER" ]] || isNotMalice || (( $+ialiases[$LBUFFER] )) || { (( $+aliases[$LBUFFER] )) && zle expand-aliases-widget } #_expand_alias
    [[ "$insertBlank" = "0" ]] || zle self-insert
}
isNotMalice || {
    zle -N expand-alias-space
    bindkey " " expand-alias-space
    bindkey -M isearch " " magic-space
}
