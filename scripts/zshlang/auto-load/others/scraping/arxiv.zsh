##
function arxiv-exportify {
    in-or-args "$@" |
        perl -lpe 's|^https://(arxiv\.org)|https://export.$1|gi' |
        cat-copy-if-tty
}

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
    urls_source=()
    urls_abs=()
    urls_vanity=()
    urls_semantic_scholar=()
    doi_ids=()
    ##
    local urls
    urls="$(in-or-args "$@")" @RET
    urls=(${(@f)urls})
    local redirect_p="${arxiv_url_get_redirect_p:-y}"

    local url id doi_id acl_id retcode=0
    for url in $urls[@] ; do
        local redirected_p=''
        while true ; do
            id=""
            if [[ "$url" =~ '(?i)/(?:abs|pdf)/(?:arxiv:)?([^/]+?)(?:\.pdf)?(?:#.*)?/*$'
                    || "$url" =~ '(?i)arxiv:([^/]+?)(?:\.pdf)?/*$'
                    || "$url" =~ '(?i)huggingface\.co/papers/([^/]+)'
                    || "$url" =~ '(?i)(?:ar5iv\.labs\.)?arxiv\.org/html/(\d+\.\d+)'
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
            elif ! bool "${redirected_p}" ; then
                redirected_p=y
                url="$(revaldbg urlfinalg "$url")" @TRET
                continue
            else
                ecerr "$0: bad URL $(gq "$url")"
                retcode=1

                break
            fi
            ids+="$id"
            urls_pdf+="https://export.arxiv.org/pdf/${id}"
            urls_source+="https://export.arxiv.org/e-print/${id}"
            urls_abs+="https://export.arxiv.org/abs/${id}"
            urls_vanity+="https://arxiv-vanity.com/papers/${id}"
            urls_semantic_scholar+="https://api.semanticscholar.org/arXiv:${id}"

            break
        done
    done

    arrnn ${(@)urls_abs} | cat-copy-if-tty @RET

    return $retcode
}

function arxiv-json {
    #: Use =tar xf $file --directory=$name= on the downloaded files.
    ##
    local inargs
    in-or-args3 "$@" @RET

    local url ids api_endpoint title
    for url in ${inargs[@]}; do
        ids=()
        assert sout arxiv-url-get "$url" @RET

        for id in ${ids[@]} ; do
            api_endpoint="http://export.arxiv.org/api/query?id_list=${id}"
            # re var-show id api_endpoint

            title="$(curl -s "$api_endpoint")" @TRET
            #  | ggrep -oPm1 "(?<=<title>)[^<]+"
            ec "${title}" | xml2json
        done
    done
}

function arxiv-title {
    arxiv-json "$@" |
        jqm '.feed.entry.title'
}

function h-arxiv-source-dl {
    local inargs
    in-or-args3 "$@" @RET

    local url url_dl urls_source title title_fs archive
    for url in ${inargs[@]}; do
        urls_source=()
        assert sout arxiv-url-get "$url" @RET

        title="$(arxiv-title "$url")" @TRET
        title_fs="$(str2filename "$title")" @TRET

        for url_dl in ${urls_source[@]} ; do
            ##
            # reval-ec url-filename "${url_dl}"
            #: fails

            archive="${title_fs}.tar.gz"
            assert reval-ec wgetm --continue -O  "${archive}" "${url_dl}" @RET

            mkdir-m "${title_fs}"
            assert reval-ec gtar --directory="${title_fs}" --extract --ungzip --verbose --file="${archive}" @RET

            trs "${archive}" || true
            ##
        done
    done
}

function arxiv-source-dl {
    #: now @global
    # local paper_source_dir

    reval-ecgray pushf "${paper_source_dir}" @RET
    {
        h-arxiv-source-dl "$@"
    } always { popf }
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
function arxiv-latex-dl {
    local inargs
    in-or-args3 "$@" @RET

    #: now @global
    # local paper_dir

    local url paper_id api_url paper_title
    for url in ${inargs[@]}; do
        paper_id="$(ec "$url" | perl -nE 'say /\/(\d+\.\d+)$/')" @TRET

        # Fetch paper's title using arXiv API
        api_url="http://export.arxiv.org/api/query?id_list=${paper_id}"
        paper_title="$(gurl "${api_url}")" @TRET

        if isDbg ; then
            i="${paper_title}"

            ec-sep-h
            ecgray "${paper_title}"
            ec-sep-h
        fi

        # Use jq to extract the title

        paper_title="$(ec "${paper_title}" | xml2 | rget '/feed/entry/title=(.+)' | str2filename)" @TRET

        local save_dir="${paper_source_dir}/${paper_title}"
        mkdir-m "${save_dir}"

        # Download LaTeX source
        local download_url="https://export.arxiv.org/e-print/${paper_id}"
        local archive="${save_dir}/src.tar.gz"
        assert reval-ec wgetm -q "${download_url}" -O "${archive}" @RET

        # Unzip the content
        assert command tar -xzvf "${archive}" -C "${save_dir}" @RET

        # Remove the downloaded tar.gz file
        trs-rm "${archive}" @STRUE

        ecgray "Downloaded and extracted to ${save_dir}"
    done
}
##
function arxiv-id-get {
    local ids=()
    assert sout arxiv-url-get "$@" @RET

    arrnn "${ids[@]}" |
        cat-copy-if-tty
}

function h-emc-arxiv-id-get {
    arxiv_url_get_redirect_p=n serr arxiv-id-get "$@" || true
}
##
