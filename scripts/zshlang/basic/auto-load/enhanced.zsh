re 'self-enh enh-mkdest' ln
##
cp() {
    local emd_c='command gcp'
    enh-mkdest "$@"
}
##
h_mv() {
    local emd_c='command mv'
    if isIReally && isRcLoaded ; then
        emd_c='mv-merge'
        color 170 170 170 "mv: using mv-merge instead"
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
        h_mv -v -- "$1" "$newfilename" @RET
        out="$newfilename"
    fi
}
mv2 () {
    (( $#@ < 2 )) && { ecerr "Usage: mv2 <dest> <path> ..." ; return 1 }
    reval-ec mv "${@[-1]}" "${@[1,-2]}"
}
##
alias noglob='noglob ruu ""'
##
