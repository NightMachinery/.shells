##
function semantic-scholar-url-get {
    local urls_semantic_scholar=() doi_ids=()
    sout arxiv-url-get "$@" @TRET
    arrNN $urls_semantic_scholar[@] | cat-copy-if-tty
}
aliasfn ssu semantic-scholar-url-get

function arxiv-url-get {
    ## @global/outputs
    ids=()
    urls_pdf=()
    urls_abs=()
    urls_vanity=()
    urls_semantic_scholar=()
    doi_ids=()
    ##
    local urls
    urls="$(in-or-args "$@")" @RET
    urls=(${(@f)urls})

    local url id doi_id acl_id retcode=0
    for url in $urls[@] ; do
        local redirected_p=''
        while true ; do
            id=""
            if [[ "$url" =~ '(?i)/(?:abs|pdf)/(?:arxiv:)?([^/]+?)(?:\.pdf)?(?:#.*)?/*$'
                    || "$url" =~ '(?i)arxiv:([^/]+?)(?:\.pdf)?/*$'
                    || "$url" =~ '(?i)ar5iv.labs.arxiv.org/html/(\d+\.\d+)'
                    || "$url" =~ '(?i)semanticscholar.org/arxiv:([^/]+?)/*$'
                    || "$url" =~ '(?i)^https://scholar.google.com/.*&arxiv_id=([^/&]+)/*$'
                    || "$url" =~ '^https://(?:www\.)?doi\.org(?:.*)/arXiv\.([^/]+)'
                    || "$url" =~ '.*/(\d+\.\d+)\.pdf$'
                  ]] ; then
                #: @example https://doi.org/10.48550/arXiv.2012.07532
                id="${match[1]}"
            elif [[ "$url" =~ '^(https://(?:www\.)?(?:api\.)?semanticscholar\.org/[^?]+)' ]] ; then
                urls_semantic_scholar+="${match[1]}"
                break
            elif [[ "$url" =~ '^https://(?:www\.)?scholar\.google\.' ]] ; then
                local html
                html="$(full-html2 "$url")" @TRET
                if id="$(ec "$html" | rget '(?i)https?://arxiv.org/(?:abs|pdf)/(?:arxiv:)?([^/]+?)(?:\.pdf)?(?:/|")' | head -n 1)" ; then
                else
                    break
                fi
            elif [[ "$url" =~ '^https://(?:www\.)?doi\.org/(.*)' ]] ; then
                #: @example https://doi.org/10.18653/v1/D19-1221

                doi_id="${match[1]}"

                doi_ids+="${doi_id}"
                # urls_semantic_scholar+="https://api.semanticscholar.org/graph/v1/paper/${doi_id}"
                urls_semantic_scholar+="https://api.semanticscholar.org/${doi_id}"
                break
            elif [[ "$url" =~ '^https://(?:www\.)?aclanthology\.org/([^/]*)' || "$url" =~ '^https://(?:www\.)?aclweb\.org/anthology/(?:.*/)?([^/]+)' ]] ; then
                #: @example https://aclanthology.org/2020.acl-main.431/
                #: @example https://www.aclweb.org/anthology/P19-1580/
                #: @example https://aclweb.org/anthology/papers/N/N19/N19-1357/
                acl_id="${match[1]}"
                acl_id="$(ec "$acl_id" | perl -ple 's/\.pdf$//g' )" @TRET
                urls_semantic_scholar+="https://api.semanticscholar.org/ACL:${acl_id}"
                break
            elif ! bool "$redirected_p" ; then
                url="$(urlfinalg "$url")" @TRET
                continue
            else
                ecerr "$0: bad URL $(gq "$url")"
                retcode=1
            fi
            ids+="$id"
            urls_pdf+="https://arxiv.org/pdf/${id}"
            urls_abs+="https://arxiv.org/abs/${id}"
            urls_vanity+="https://arxiv-vanity.com/papers/${id}"
            urls_semantic_scholar+="https://api.semanticscholar.org/arXiv:${id}"

            break
        done
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
