##
function semantic-scholar-url-get {
    local urls_semantic_scholar=()
    sout arxiv-url-get "$@" @TRET
    arrNN $urls_semantic_scholar[@] | cat-copy-if-tty
}

function arxiv-url-get {
    ## @global/outputs
    ids=()
    urls_pdf=()
    urls_abs=()
    urls_vanity=()
    urls_semantic_scholar=()
    ##
    local urls
    urls="$(in-or-args "$@")" @RET
    urls=(${(@f)urls})

    local url id retcode=0
    for url in $urls[@] ; do
        id=""
        if [[ "$url" =~ '(?i)/(?:abs|pdf)/(?:arxiv:)?([^/]+?)(?:\.pdf)?/*$'
              || "$url" =~ '(?i)semanticscholar.org/arxiv:([^/]+?)/*$'
              || "$url" =~ '(?i)^https://scholar.google.com/.*&arxiv_id=([^/&]+)/*$'
            ]] ; then
            id="${match[1]}"
        ##
        elif [[ "$url" =~ '^https://(?:www\.)?api\.semanticscholar\.org' ]] ; then
            urls_semantic_scholar+="$url"
            continue
        ##
        else
            ecerr "$0: bad URL $(gq "$url")"
            retcode=1
        fi
        ids+="$id"
        urls_pdf+="https://arxiv.org/pdf/${id}"
        urls_abs+="https://arxiv.org/abs/${id}"
        urls_vanity+="https://arxiv-vanity.com/papers/${id}"
        urls_semantic_scholar+="https://api.semanticscholar.org/arXiv:${id}"
    done

    arrnn ${(@)urls_abs} | cat-copy-if-tty @RET

    return $retcode
}

function arxiv-dl {
    local url="${1}" #: Example: =https://arxiv.org/abs/2109.02355=
    assert-args url @RET
    local dest="${arxiv_dl_o}"
    local mode="${arxiv_dl_mode:-pdf}"

    local ids urls_pdf urls_abs urls_vanity title
    arxiv-url-get "$url" @RET
    if [[ "$mode" == 'pdf' ]] ; then
        if test -z "$dest" ; then
            title="$(url-title "$url" | str2filename)" @TRET
            dest="${title}.pdf"
        fi

        revaldbg aa-2dest "${urls_pdf[1]}" "$dest" >&2 @TRET
        ec "$dest"
    elif [[ "$mode" == 'vanity' ]] ; then
        web2pdf "${urls_vanity[1]}"
    fi
}
renog arxiv-dl

aliasfn-ng arxiv-vanity arxiv_dl_mode=vanity arxiv-dl
@opts-setprefix arxiv-vanity web2pdf
aliasfn-ng arxv arxiv-vanity
@opts-setprefix arxv web2pdf

function arxiv-k2 {
    local url="${1}"
    assert-args url @RET

    local dled
    dled="$(arxiv_dl_mode=pdf arxiv-dl "$url")" @TRET
    k2pdf "$dled" @TRET
    local converted
    converted="${dled:r}_k2opt.pdf"
    assert test -e "$converted" @RET
    2ko "$converted" @TRET
}
renog arxiv-k2
##
