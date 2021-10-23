##
function scan {
    local i="$1" res="${scan_res:-300}" scanner="${scan_s:-brother}"
    assert-args i @RET
    local format="${scan_f:-jpeg}"
    local dir="${i:h}"
    i="${i:t}"
    ensure-array scan_opts
    local opts=("${scan_opts[@]}")

    if [[ "$format" == (jpeg|tiff) ]] ; then
        opts+=(-"$format")
    elif [[ "$format" == avif ]] ; then
        opts+=(-tiff)
    fi


    reval-ec scanline -dir "$dir" -flatbed -verbose -a4 -scanner "$scanner" -resolution "$res" "$opts[@]" -name "$i" @TRET

    if [[ "$format" == avif ]] ; then
        tiff2avif "$i" @TRET
        trs "$i"
    fi
}
##
function multicrop {
    : "usage: input [output]"

    local i="${1}"
    assert-args i @RET
    local ext="${multicrop_ext:-${${i:e}:-png}}"
    local o="${2:-${i:r}_cropped.${ext}}"
    local discard="${multicrop_d:-400}"
    local fuzzy="${multicrop_f:-10}"
    local prune="${multicrop_p}" # try 10
    local bg="${multicrop_bg:-white}"
    local del="${multicrop_del}"
    ensure-array multicrop_opts
    local opts=("${multicrop_opts[@]}")
    if test -n "$prune" && (( $prune > 0 )) ; then
        opts+=(-p "$prune")
    fi

    icat "$i"

    local tmp_dir tmp # multicrop.bash is buggy in handling output names, so we use temp files as a crutch
    tmp_dir="$(gmktemp --directory)" @TRET
    tmp="${tmp_dir}/${o:t}"
    reval-ec multicrop.bash -b "$bg" -d "$discard" -f "$fuzzy" "$opts[@]" "$i" "$tmp" @TRET
    local output_files
    output_files=( "$tmp_dir"/*(DN) )
    if (( ${#output_files} == 0 )) ; then
        ecerr "$0: no output produced for $(gquote-sq "$i"); aborting."
        return 1
    fi

    re icat "$output_files[@]"

    # you can use `command yes | ...` to force gmv to overwrite
    gmv -v -i "$output_files[@]" "${o:h}" @TRET

    if bool-ask "$del" "$0: delete original file $(gquote-sq "$i")?" Y ; then
        trs "$i"
    fi
}

function innercrop {
    : "usage: input [output]"

    local i="${1}"
    assert-args i @RET
    local ext="${innercrop_ext:-${${i:e}:-png}}"
    local o="${2:-${i:r}_cropped.${ext}}"
    local fuzzy="${innercrop_f:-10}" # @weird higher values can cause less crop (it's nonlinear)
    local bg="${innercrop_bg:-white}"
    ensure-array innercrop_opts
    local opts=("${innercrop_opts[@]}")
    local del="${innercrop_del}"


    icat "$i"
    reval-ec command innercrop.bash -o "$bg" -f "$fuzzy" "$opts[@]" "$i" "$o" @TRET
    icat "$o"

    if bool-ask "$del" "$0: delete original file $(gquote-sq "$i")?" Y ; then
        trs "$i"
    fi
}

function autotrim {
    : "usage: input [output]"

    local i="${1}"
    assert-args i @RET
    local ext="${autotrim_ext:-${${i:e}:-png}}"
    local o="${2:-${i:r}_cropped.${ext}}" o_supplied="$2"
    local fuzzy="${autotrim_f:-40}"
    local bg="${autotrim_bg:-white}"
    ensure-array autotrim_opts
    local opts=("${autotrim_opts[@]}")
    local del="${autotrim_del}"


    icat "$i"
    reval-ec command autotrim.bash -C "$bg" -f "$fuzzy" "$opts[@]" "$i" "$o" @TRET
    icat "$o"

    if bool-ask "$del" "$0: delete original file $(gquote-sq "$i")?" Y ; then
        if test -n "$o_supplied" ; then
            trs "$i"
        else
            gmv -v "$o" "$i" @TRET
        fi
    fi
}
##
