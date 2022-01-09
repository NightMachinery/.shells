##
function h-from-imdb {
    local urls
    urls="$(cat-paste-if-tty)" @RET
    local mode="${h_from_imdb_m}"
    assert-args mode @RET

    local ids
    ids=(${(@f)"$(ec "$urls" | imdb-ids-get)"}) @TRET

    {
    local id
    for id in ${ids[@]} ; do
        if [[ "$mode" == rarbg ]] ; then
            ec "https://rarbg.to/torrents.php?search=${id}&order=seeders&by=DESC"
        elif [[ "$mode" == zooqle ]] ; then
            ec "https://zooqle.com/search?q=${id}"
        else
            ecerr "$0: unsupported mode: $(gquote-sq "$mode")"
            return 1
        fi
    done
    } | cat-copy-if-tty
}

aliasfn rarbg-from-imdb h_from_imdb_m=rarbg h-from-imdb

aliasfn zooqle-from-imdb h_from_imdb_m=zooqle h-from-imdb
##
