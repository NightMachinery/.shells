function hn2org {
    local url="$1"
    local title
    title="$(url-title "$url" | str2pandoc-title)" @TRET
    local dest="${2}"
    if test -z "$dest" ; then
        dest=~ktmp/hn/"$title".org
        ensure-dir "$dest" @TRET
    fi
    if ! { test -d "$dest" || [[ "$dest" == *.org ]] } ; then
        dest+='.org'
    fi

    local emc_dest
    emc_dest="$(emc-eval "(hn/to-org-mode! $(emc-quote "$url") $(emc-quote "$dest"))")" @TRET
    dvar emc_dest
    if test -d "$dest" ; then
        dest="$emc_dest"
    fi

    grealpath -e "$dest"
}

function hn2epub {
    local url="$1"

    local org_file
    org_file="$(reval-ec hn2org "$url")" @TRET

    @opts url "$url" author 'HN' @ org2epub-auto-metadata "$org_file"
}
renog hn2epub
##
