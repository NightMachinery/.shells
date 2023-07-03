function h-org-dest-from-url {
    local url="$1" dest="${2}"
    if [[ "$dest" == '-' ]] ; then
        ec "$dest"
        return $?
    fi
    assert-args url @RET

    local title
    title="$(url-title "$url" | str2pandoc-title)" @TRET

    if test -z "$dest" ; then
        dest=~ktmp/org/"$title".org
        ensure-dir "$dest" @TRET
    else
        ensure-dir "$dest" @TRET
        if test -d "$dest" ; then
            dest+=/"$title".org
        elif ! [[ "$dest" == *.org ]] ; then
            dest+='.org'
        fi
    fi

    ec "$dest"
}
##
function hn2org {
    local url="$1" dest="${2}"
    assert-args url @RET
    dest="$(h-org-dest-from-url "$url" "$dest")" @TRET

    local emc_dest
    emc_dest="$(emc-eval "(hn/to-org-mode! $(emc-quote "$url") $(emc-quote "$dest"))")" @TRET
    dvar emc_dest
    if test -d "$dest" ; then
        dest="$emc_dest"
    fi

    grealpath -e -- "$dest"
}

function hn2epub {
    local url="$1"

    local org_file
    org_file="$(reval-ec hn2org "$url")" @TRET

    @opts url "$url" author 'HN' @ org2epub-auto-metadata "$org_file"
}
renog hn2epub
##
