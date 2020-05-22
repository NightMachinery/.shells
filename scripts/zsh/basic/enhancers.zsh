function enh-savename() {
    mdoc "$0 <name of original function> <its renamed version after enhancement>" MAGIC

    typeset -A -g enhSavedNames
    test -n "${enhSavedNames[$1]}" || enhSavedNames[$1]="$2"
}
function enh-mkdest() {
    doc enhances commands by creating directories for destination.
    local dest="${@: -1}"
    ecdbg "dest: $dest   bdest: $(bottomdir "$dest")"
    mkdir -p "$(bottomdir "$dest")"
    ruu "${=emd_c:-comment}" "$@"
}
function self-enh() {
    eval "function \\$2() emd_c='command $2' $1" '"$@"'
}
function nig() {
    doc Use alias 'sii'
    doc Skips the first word if interactive MAGIC
    isI && eval "$(gquote "${@:2}")" || "$1" "${@:2}"
}
function nulterm() {
    reval "$@"
    ec $'\0'
}
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
    seval "$f[@]" "$=a" "$(gquote "${@:3}")"
}
noglobfn() {
    doc Prepends noglob to functions. You need to define the original function in quotes if you want to reload the function definition in the future.

    (( ${+aliases[$1]} )) && unalias "$1"
    {
        local realname="_noglob_$1"
        enh-savename "$1" "$realname"
        functions[$realname]=$functions[$1]
        alias "$1"="\noglob $realname"
    }
    #unfunction "$1"
    # if anyone uses the previous version they are probably not needing a noglob so let them be
}
function reify() {
    doc "Makes a single argument function work for multiple args by redifining it and using run-on-each."

    test -n "$functions[$1]" || { ecerr "Function '$1' is empty or doesn't exist." ; return 1 }

    local realname="_reify_$1"
    local enhanced="run-on-each $realname"' "$@"'
    [[ "$functions[$1]" =~ '\s*\Q'"$enhanced"'\E\s*' ]] || {
        enh-savename "$1" "$realname"
        functions[$realname]=$functions[$1]
        functions[$1]="$enhanced"
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
