##
function hear-rm {
    local i
    i="$(hear-get)" @TRET

    if ask "Delete $(gq "$i")?" Y ; then
        hear-next || true
        trs "$i"
    fi
}
##
function h-hear-do {
    local engine="${hear_do_engine:-mv}"
    local copy_mode="${hear_do_copy_mode:-org}"
    local opts=("${@[1,-2]}") dest="${@[-1]}"

    local i
    i="$(hear-get)" @TRET

    assert reval-ec "${engine}" "${opts[@]}" "$i" "${dest}" @RET
    if test -d "$dest" ; then
        dest="${dest}/${i:t}"
        assert test -f "$dest" @RET
    fi

    if [[ "${copy_mode}" == org ]] ; then
        pbcopy "[[audiofile:$(org-escape-link "${dest}" | path-abbrev)]]"
    fi
}

function hear-mv {
    hear_do_engine=mv h-hear-do "$@"
}

function hear-cp {
    hear_do_engine=cp h-hear-do "$@"
}

function hear-cp2misc {
    hear-cp ~mu/mmisc/misc
}
##
function hear-tag {
    local tags=($@)
    ntag-add "$(hear-get)" "${tags[@]}"
}
aliasfn hear-blue hear-tag blue
aliasfn hear-gray hear-tag gray
aliasfn air-blue hear-blue
##
