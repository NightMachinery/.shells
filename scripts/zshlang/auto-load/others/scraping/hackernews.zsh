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
    # @todo0 refactor `org2epub-auto-metadata' out of this
    ##
    local url="$1"

    local org_file
    org_file="$(reval-ec hn2org "$url")" @TRET

    local title
    title="${org_file:t:r}"
    title="$(ec "$title" | str2pandoc-title)" @TRET
    assert-args title @RET
    local reading_est
    reading_est="$(cat "$org_file" | count-words-humanfriendly)" @STRUE
    local url_date
    url_date="$(url-date "$url")" || true

    local epub_file
    epub_file="${org_file:h}/${title}.epub"
    epub_file="$(reval-env-ec pandoc_convert_dest="${epub_file}" org2epub-pandoc "$title" "[${reading_est}] HN $url_date" "$org_file")" @TRET

    epub_file="$(reval-ec p2k "${epub_file}")" @TRET

    grealpath -e "$epub_file"
}

renog hn2epub
##
