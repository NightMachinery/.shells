re 'self-enh enh-mkdest' ln
##
function cp() {
    local emd_c='command gcp --reflink=auto' # --reflink=auto does copy-on-write copies
    enh-mkdest "$@"
}
##
function h_mv() {
    local emd_c='command mv'
    if isIReally && isRcLoaded && fn-isTop mv ; then
        emd_c='mv-merge'
        ecgray "mv (top-level): using mv-merge instead" >&2
    fi
    enh-mkdest "$@"
}

function mv {
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

        ensure-dir "$newfilename"
        command gmv -v -- "$1" "$newfilename" @RET
        out="$newfilename"
    fi
}
##
function enh-dest-shift () {
    local opts=() engine=("${enh_dest_shift_e[@]}")
    assert-args engine @RET
    while [[ "$1" == -* ]] ; do
        opts+="$1"
        shift

        if [[ "${opts[-1]}" == '--' ]] ; then
            break
        fi
    done

    (( $#@ < 2 )) && { ecerr "Usage: <dest> <path> ..." ; return 1 }

    reval-ec "${engine[@]}" "${opts[@]}" "${@[2,-1]}" "$1"
}
aliasfn ln2 enh_dest_shift_e=(ln) enh-dest-shift
aliasfn mv2 enh_dest_shift_e=(mv) enh-dest-shift
aliasfn cp2 enh_dest_shift_e=(cp -r) enh-dest-shift
##
# alias noglob='noglob ruu ""'
##
