##
function ipynb2org {
    local input="$1"
    assert-args input @RET

    local dest="${2}"
    if test -z "$dest"; then
        dest="${input:r}.org"
        if test -e "$dest" ; then
            if ask "$0: overwrite destination: $dest" n ; then
                trs "$dest"
            else
                return 1
            fi
        fi
    fi

    ecgray "$0: WARNING This conversion is not lossless. In particular, it seems to drop backslashes sometimes (in Python strings?)."

    local tmp
    tmp="$(mktemp)" @TRET
    {
        assert revaldbg jupyter nbconvert "$input" --stdout --to markdown > "$tmp" @RET
        { cat "$tmp" | md2org > "$dest" } @TRET

        realpath "$dest"
    } always { silent trs-rm "$tmp" }
}
##
