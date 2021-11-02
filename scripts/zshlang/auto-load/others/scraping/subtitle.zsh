##
function sub-dl {
    local f="$1"
    # local engine="${sub_dl_e:-subliminal}"
    local engine="${sub_dl_e:-subscene}"
    assert-args f @RET

    local tmp d
    if tmp="$(grealpath -e "$f")" ; then
        f="$tmp"
        d="${f:h}"
    else
        d="$PWD"
    fi

    if [[ "$engine" = subscene ]] ; then
        reval-ec indir-exists "$d" subscene-dl "${f:t:r}"
    elif [[ "$engine" = subliminal ]] ; then
        reval-ec indir-exists "$d" subliminal download -f -l en -d "$d" "$f"
    fi
}

function subscene-dl {
    local name="$*" count="${sub_dl_c:-10}"

    local opts=()
    if ! isIReally ; then
        opts+=(--silent) # non-interactive mode, fails more aggresively than the interactive mode
    fi
    reval-ec subgrab "${opts[@]}" -m "$name" -l en -c "$count"
}
@opts-setprefix subscene-dl sub_dl
##
