re 'self-enh enh-mkdest' cp ln
_mv() {
    emd_c='command mv' enh-mkdest "$@"
}
mv () {
    local out
    args-nochromefile "$@"
    set -- "${out[@]}"
    if [ "$#" -ne 1 ] || [ ! -e "$1" ]; then
        _mv "$@"
    else
        local newfilename="$1"
        vared newfilename || {
            ecerr "$0: Canceled"
            return 1
        }
        # bash: read -ei "$1" newfilename
        _mv -v -- "$1" "$newfilename"
    fi
}
alias noglob='noglob ruu ""'
watchm() {
    ruu "watch -n $1" "${@:2}"
}
