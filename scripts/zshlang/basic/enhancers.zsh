##
function enh-savename() {
    : "<name of original function> <its renamed version after enhancement>"
    # @design Adding a way to keep track of all saved names would be good.

    local n1="$1" n2="$2"
    test -z "$2" && return 1


    typeset -A -g enhSavedNames

    test -n "${enhSavedNames[$n1]}" || {
        if isdbg && ! (( $+commands[$n2] || $+functions[$n2] || $+aliases[$n2] )) ; then
            print -r -- "WARNING $0: n1=$n1; n2=$n2 is probably invalid. (Use enh-savename after creating the renamed version, so we can verify that it exists.)" >&2
        fi
        enhSavedNames[$n1]="$n2"
    }
}

function preEnhNames() {
    unset out
    : "usage: args ...
Replaces any arg that is in enhSavedNames with its original name. Outputs in \$out.
Update: Actually adds, not replaces. Also removes duplicates."

    local items=( "$@" )
    local itemsP=()
    for item in "$items[@]"
    do
        local on="$enhSavedNames[$item]"
        itemsP+="$item"
        if test -n "$on" ; then
            itemsP+="$on"
        else

        fi
    done
    out=("${(u@)itemsP}")
}
##
function enh-mkdest() {
    doc enhances commands by creating directories for destination.

    local dest="${@: -1}"

    ensure-dir "$dest"

    # ruu "${=emd_c:-comment}" "$@" # Old API
    revaldbg "${=emd_c:-comment}" "$@"
}

function self-enh() {
    eval "function \\$2() emd_c='command $2' $1" '"$@"'
}
##
function nig() {
    doc Use alias 'sii'
    doc 'Skips the first word if interactive MAGIC'

    isI && eval "$(gquote "${@:2}")" || "$1" "${@:2}"
}
##
function nulterm() {
    reval "$@"
    ec $'\0'
}
##
function expand-alias-strip() {
    # FNSWAP: expand-aliases (in force-expand)
    local a="$(force-expand "$1")"
    comment @lilbug strip-left
    a="$(strip "$a" '\\?noglob ')"
    a="$(strip "$a" '\\?nocorrect ')"
    a="$(strip "$a" 'ruu "" ')"
    ec "$a"
}

function ruu() {
    doc helper function to expand aliases for commands like sudo, nohup, etc
    local f=()
    [[ "$1" =~ '^\s*$' ]] || f+="${=1}"
    local a="$(expand-alias-strip "$2")"

    # @experimental
    evaldbg "$(gq "$f[@]")" "$a" "$(gq "${@:3}")"
}
##
function noglobfn() {
    doc Prepends noglob to functions. You need to define the original function in quotes if you want to reload the function definition in the future.

    (( ${+aliases[$1]} )) && unalias "$1"
    {
        local realname="h_noglob_$1"
        functions[$realname]=$functions[$1]
        alias "$1"="\noglob $realname"
        enh-savename "$1" "$realname"
    }
    #unfunction "$1"
    # if anyone uses the previous version they are probably not needing a noglob so let them be
}
##
function reify() {
    doc "Makes a single argument function work for multiple args by redifining it and using run-on-each."

    test -n "$functions[$1]" || { ecerr "Function '$1' is empty or doesn't exist." ; return 1 }

    local realname="h_reify_$1"
    local enhanced="run-on-each $realname"' "$@"'
    [[ "$functions[$1]" =~ '\s*\Q'"$enhanced"'\E\s*' ]] || {
        functions[$realname]=$functions[$1]
        functions[$1]="$enhanced"
        enh-savename "$1" "$realname"
    }
}
reify reify
reify noglobfn

function renog() {
    local i
    for i in "$@" ; do
        reify "$i"
        noglobfn "$i"
    done
}
##
function enh-urlfinal() {
    doc "Run urlfinalg on args on the given function automatically."

    test -n "$functions[$1]" || { ecerr "Function '$1' is empty or doesn't exist." ; return 1 }

    local realname="h_urlfinal_$1"
    local enhanced="transformer urlfinalg $realname"' "$@"'
    [[ "$functions[$1]" =~ '\s*\Q'"$enhanced"'\E\s*' ]] || {
        enh-savename "$1" "$realname"
        functions[$realname]=$functions[$1]
        functions[$1]="$enhanced"
    }
}
##
function paste-after() { # paste
    pbpaste-plus # outputs in `paste`
    if test -n "$paste[*]" ; then
        @opts e geval @ reval-env "$@" "$paste[@]"
        return $?
    else
        ecerr "$0: Clipboard was empty."
        return 1
    fi
}

function p {
    if (( $#@ == 0 )) ; then
        pbpaste
    else
        paste-after "$@"
    fi
}
##
function enh-addfinder() {
    local sel
    sel=("${(@f)$(finder-sel-get)}") || return $?
    test -z "$sel[*]" && return 1
    rgeval "$@" $sel[@]
}
alias pf='enh-addfinder'
##
