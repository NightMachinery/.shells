function sub-dl {
    local f="$1"
    assert-args f @RET

    local tmp d
    if tmp="$(grealpath -e "$f")" ; then
        f="$tmp"
        d="${f:h}"
    else
        d="$PWD"
    fi

    reval-ec subliminal download -f -l en -d "$d" "$f"
}
