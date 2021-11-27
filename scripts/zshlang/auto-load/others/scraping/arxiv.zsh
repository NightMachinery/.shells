##
function arxiv-dl {
    local url="${1}" #: Example: =https://arxiv.org/abs/2109.02355=
    assert-args url @RET
    local dest="${arxiv_dl_o}"
    local mode="${arxiv_dl_mode:-pdf}"

    local id url_pdf url_abs url_vanity title
    if [[ "$url" =~ '(?i)/(?:abs|pdf)/(?:arxiv:)?([^/]+?)(?:\.pdf)?/*$'
          || "$url" =~ '(?i)semanticscholar.org/arxiv:([^/]+?)/*$'
          || "$url" =~ '(?i)^https://scholar.google.com/.*&arxiv_id=([^/&]+)/*$'
        ]] ; then
        id="${match[1]}"
        url_pdf="https://arxiv.org/pdf/${id}"
        url_abs="https://arxiv.org/abs/${id}"
        url_vanity="https://arxiv-vanity.com/papers/${id}"


        if [[ "$mode" == 'pdf' ]] ; then
            if test -z "$dest" ; then
                title="$(url-title "$url" | str2filename)" @TRET
                dest="${title}.pdf"
            fi

            revaldbg aa-2dest "$url_pdf" "$dest" >&2 @TRET
            ec "$dest"
        elif [[ "$mode" == 'vanity' ]] ; then
            web2pdf "$url_vanity"
        fi
    else
        ecerr "$0: Bad URL $(gq "$url")"
        return 1
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
renog arxiv2k
##
