re 'self-enh enh-mkdest' ln
##
cp() {
    local emd_c='command gcp --reflink=auto' # --reflink=auto does copy-on-write copies
    enh-mkdest "$@"
}
##
h_mv() {
    local emd_c='command mv'
    if isIReally && isRcLoaded && fn-isTop mv ; then
        emd_c='mv-merge'
        ecgray "mv (top-level): using mv-merge instead" >&2
    fi
    enh-mkdest "$@"
}
mv () {
    unset out # out is GLOBAL
    args-nochromefile "$@"
    set -- "${out[@]}"
    unset out
    if [ "$#" -ne 1 ] || [ ! -e "$1" ]; then
        h_mv "$@"
    else
        local newfilename="$1"
        vared newfilename || {
            ecerr "$0: Canceled"
            return 1
        }
        # bash: read -ei "$1" newfilename
        command gmv -v -- "$1" "$newfilename" @RET
        out="$newfilename"
    fi
}
mv2 () {
    (( $#@ < 2 )) && { ecerr "Usage: mv2 <dest> <path> ..." ; return 1 }
    reval-ec mv "${@[2,-1]}" "$1"
}
cp2 () {
    (( $#@ < 2 )) && { ecerr "Usage: cp2 <dest> <path> ..." ; return 1 }
    reval-ec cp -r "${@[2,-1]}" "$1"
}
##
# alias noglob='noglob ruu ""'
##
