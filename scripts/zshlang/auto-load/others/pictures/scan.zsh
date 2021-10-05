##
function multicrop {
    : "usage: input [output]"

    local i="${1}"
    assert-args i @RET
    local ext="${multicrop_ext:-${${i:e}:-png}}"
    local o="${2:-${i:r}_cropped.${ext}}"
    local discard="${multicrop_d:-300}"
    local fuzzy="${multicrop_f:-10}"
    local prune="${multicrop_p:-0}" # try 10
    local bg="${multicrop_bg:-white}"
    local del="${multicrop_del}"
    ensure-array multicrop_opts
    local opts=("${multicrop_opts[@]}")

    icat "$i"

    local tmp_dir tmp # multicrop.bash is buggy in handling output names, so we use temp files as a crutch
    tmp_dir="$(gmktemp --directory)" @TRET
    tmp="${tmp_dir}/${o:t}"
    reval-ec multicrop.bash -b "$bg" -d "$discard" -f "$fuzzy" -p "$prune" "$opts[@]" "$i" "$tmp" @TRET
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
##
