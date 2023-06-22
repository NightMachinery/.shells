##
function h-from-imdb {
    local urls
    urls="$(in-or-args "$@")" @RET
    local mode="${h_from_imdb_m}"
    assert-args mode @RET

    local ugrep_opts=()

    local ids
    ids=(${(@f)"$(ec "$urls" | imdb-ids-get)"}) @TRET

    local isColorTty='false'
    if isColorTty
    then
        isColorTty='true'
    fi

    {
    local id
    for id in ${ids[@]} ; do
        if [[ "$mode" == rarbg ]] ; then
            ##
            # ec "https://rarbg.to/torrents.php?search=${id}&order=seeders&by=DESC"
            ##
            fnswap isColorTty "${isColorTty}" rarbg-ug "${id}"
            ##
        elif [[ "$mode" == zooqle ]] ; then
            ec "https://zooqle.com/search?q=${id}"
        elif [[ "$mode" == tpb ]] ; then
            ec "https://thebay.cf/search.php?q=${id}&cat=207"
        else
            ecerr "$0: unsupported mode: $(gquote-sq "$mode")"
            return 1
        fi
    done
    } | cat-copy-if-tty
}

aliasfn rarbg-from-imdb h_from_imdb_m=rarbg h-from-imdb

aliasfn zooqle-from-imdb h_from_imdb_m=zooqle h-from-imdb

aliasfn tpb-from-imdb h_from_imdb_m=tpb h-from-imdb
##
function h-rarbg-ug {
    ensure-array ugrep_opts
    local query="$*"
    local ugrep_opts=("${ugrep_opts[@]}")

    if isColorTty ; then
        ugrep_opts+='--color=always'
    fi

    ugrep --perl-regexp --smart-case --bool "${ugrep_opts[@]}" -- "$query" ~[rarbg\ json]/rarbg_items.json | {
        if isColorTty ; then
            perl -pe 's/((?:h|x)(?:26(?:4|5))|HEVC|hi10|(?:1080|720|480)p)/\x1b[1;38;2;0;0;255;48;2;255;255;255m$1\x1b[0m/gi'
            #: Add `(?:\x1b\[m\x1b\[1;31m)?` to match already colored items, but note that if you don't strip this color code out, it will override our color. The color regex is also only for the color red which is the default.
        else
            cat
        fi
    }
}
alias rarbg-ug='noglob fnswap isColorTty true h-rarbg-ug'

function h-rarbg-ug-filter {
    ensure-array ugrep_filter

    h-rarbg-ug "$@" | ugrep --perl-regexp --smart-case --color=never "${ugrep_filter[@]}"
}

function h2-rarbg-ug-filter {
    #: * @examples
    #: ** `rarbg-ug-filter '\blast\b \bof\b \bus\b s01(*nla:e\d)'`
    ##
    local query="$1" ; shift
    local ugrep_filter=(--bool -- "$@")
    if (( ${#@} > 0 )) ; then
        h-rarbg-ug-filter "$query"
    else
        h-rarbg-ug "$query"
    fi
}

alias rarbg-ug-filter='noglob fnswap isColorTty true h2-rarbg-ug-filter'

function h-rarbg-ug-265 {
    ugrep_filter=('(?:(?:x|h)265|hevc)') h-rarbg-ug-filter "$@"
}
alias rarbg-ug-265='noglob fnswap isColorTty true h-rarbg-ug-265'
##
