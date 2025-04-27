##
# * @todo We can implement a fuzzy search for papers using this API:
# ** https://api.semanticscholar.org/api-docs/graph#tag/Paper-Data/operation/get_graph_get_paper_autocomplete
#
# https://api.semanticscholar.org/graph/v1/paper/autocomplete?query=bert
#
# ** https://api.semanticscholar.org/api-docs/graph#tag/Paper-Data/operation/get_graph_get_paper_search
# https://api.semanticscholar.org/graph/v1/paper/search?query=bert
##
function semantic-scholar-to-json-api {
    #: @docs https://api.semanticscholar.org/api-docs/graph#tag/Paper-Data/operation/get_graph_get_paper
    ##
    if should-proxy-p ; then
        ##
        # ecdbg "pxa35-local"
        # pxa35-local
        ##
        pxa-local

    else
        ecdbg "$0: proxy not wanted or already active"
        var-show HTTP_PROXY
    fi

    local paper_id="${1}" #: e.g., arXiv:1705.10311
    local corpus_id="${semantic_scholar_corpus_id}"

    if [[ "$paper_id" =~ '^https://api.semanticscholar.org/((?:arXiv|CorpusID|ACL):[^/]+)$' ]] ; then
        paper_id="${match[1]}"

    ##
    # elif [[ "$paper_id" =~ '^https://www.semanticscholar.org/paper/(?:(?:[^/]+)/)?([^/]{40})(?:/)?$' ]] ; then
    elif tmp="$(semantic-scholar-id-get "$paper_id")" && test -n "$tmp" ; then
        paper_id="${tmp}"
    elif [[ "$paper_id" =~ '^http' ]] && test -n "$corpus_id" ; then
        paper_id="CorpusID:${corpus_id}"
    fi

    if [[ "$paper_id" =~ '^(arXiv:[^/]+)v\d{1,2}$' ]] ; then
        paper_id="${match[1]}"
    fi

    local json
    local url
    url="https://api.semanticscholar.org/graph/v1/paper/${paper_id}?fields=title,url,citationCount,influentialCitationCount,externalIds,abstract,venue,year,referenceCount,isOpenAccess,fieldsOfStudy,s2FieldsOfStudy,publicationTypes,publicationDate,journal,authors.name,authors.hIndex,authors.homepage,authors.affiliations,authors.citationCount,authors.paperCount,authors.url,authors.externalIds,openAccessPdf"
    #: removed from API:
    # authors.aliases

    local curl_opts_=()
    if test -n  "${SemanticScholar_API_key}"; then
        curl_opts_+=(-H "x-api-key: ${SemanticScholar_API_key}")
    fi

    json="$(revaldbg gurl "${curl_opts_[@]}" "$url")" || {
        ecerr "$0: curl-ing the API failed with $?"$'\n'"URL: ${url}"
        pbcopy "$url"
        return 1
    }
    #: =tldr= sometimes isn't available for the newer papers, and we aren't currently using it, so I have omitted it from the request above. (It will trow an error if tldr is not available.)

    ec "$json" |
        jq .
}

function semantic-scholar-to-json {
    # @alt https://www.semanticscholar.org/product/api
    ##
    local url="${1:-$(pbpaste)}"
    assert-args url @RET
    url="$(semantic-scholar-url-get "$url")" @TRET

    local json_scraped
    json_scraped="$(semantic-scholar-to-json-scraping "$url")" @TRET

    corpus_id="$(ec "$json_scraped" |
                    jqm '.corpusID' |
                    rget '(?:Corpus\s*ID:\s*)?(\d+)')" || true

    local json_api
    if ! json_api="$(semantic_scholar_corpus_id="$corpus_id" semantic-scholar-to-json-api "$url")" ; then
        ecerr "$0: API failed, falling back to scraped data"
        if true ; then
            return 1
        else
            ec "${json_scraped}"
            return 0
        fi
    fi

    if false ; then
        {
            ec "$json_scraped" | jq .
            ec '----------'
            ec "$json_api" | jq .
            ec '----------'
        } >&2
    fi

    jq -s '.[0] * .[1]' <(ec "$json_scraped") <(ec "$json_api" |
                                                    json-listify-first-level |
                                                    json-stringify-atoms)
}

function semantic-scholar-to-json-scraping {
    # @alt =semantic-scholar-to-json-api=
    ##
    local url="${1:-$(pbpaste)}"
    assert-args url @RET
    url="$(semantic-scholar-url-get "$url")" @TRET

    local html
    html="$(fhMode="${fhMode:-curlfull_s5}" full-html2 "$url")" @TRET
    #: fhMode=curl,wgetm did not work with genrouter for some reason

    ec "$html" |
        selectors2json.py \
            title '[data-selenium-selector="paper-detail-title"]' '' \
            bibtex '.bibtex-citation' '' \
            doi '[data-selenium-selector="paper-doi"] .doi__link' 'attr:href' \
            corpusID '[data-selenium-selector="corpus-id"]' '' \
            date '[data-selenium-selector="paper-year"]' '' \
            authors_names 'meta[name="citation_author"], meta[property="citation_author"]' 'attr:content' \
            journal_name '[data-heap-id="paper-meta-journal"]' '' \
            abstract_scraped 'meta[name="description"], meta[property="description"]' 'attr:content' \
            links '[data-selenium-selector="paper-link"]' 'attr:href' \
            pdf_urls 'meta[name="citation_pdf_url"], meta[property="citation_pdf_url"]' 'attr:content' \
            topics '[data-selenium-selector="entity-name"]' '' \
            referenceCount '[data-heap-nav="references"]' '' \
            | jq '.'

    #: * [jalali:1402/01/09/03:46]
    #: ** pdf_urls which is extracted using the meta tags might be the best that is scrapable without using a headless browser.
    #: ** The only PDF url the API might offer is 'openAccessPdf'.
    ##
    # abstract '[data-selenium-selector="abstract-text"]' '->org' \
        # authors '.author-list' '->org' \
        # '+1 author' appears in the authors list.
    # citationCount '[data-heap-nav="citing-papers"]' '' \
        ##
}

function semantic-scholar-to-org {
    #: This function should probably be called only with a single arg ... I doubt combining its output for mutiple URLs will be good.
    ##
    local inargs
    in-or-args3 "$@" @RET

    local url
    for url in ${inargs[@]} ; do
        url="$(semantic-scholar-url-get "$url")" @TRET

        local json
        json="$(revaldbg semantic-scholar-to-json "$url")" @TRET

        ec "$json" |
            revaldbg semantic_scholar_json_to_org.lisp "$url"
    done | cat-copy-if-tty
}
aliasfn ssorg semantic-scholar-to-org
##
function arxiv-to-org {
    local inargs
    in-or-args3 "$@" @RET

    local url
    for url in ${inargs[@]} ; do
        url="$(arxiv-url-get "$url")" @TRET

        local json
        json="$(revaldbg arxiv-json "$url")" @TRET

        ec "$json" |
            revaldbg paper_to_org.py "$url"
    done | cat-copy-if-tty
}
##
function semantic-scholar-dl-from-org {
    # if should-proxy-p ; then
    #     pxa89-local
    # fi

    local tlg_dest="${ssdl_tlg_dest:-$tlg_ch_books}"
    local tlg_p="${ssdl_tlg_p:-y}"

    local dir="${ss_dl_dir}"
    if test -z "$dir" ; then
        if isBorg ; then
            dir="$(gmktemp -d)" @TRET
        else
            dir="${paper_dir}"
            if ! test -d "$dir" ; then
                dir="."
            fi
        fi
    fi

    local org
    org="$(cat-paste-if-tty)" @TRET

    local urls
    if urls="$(ec "$org" | rget '^\s*:openAccessPdf:\s+(.*?)\s*$')" ; then
    elif urls="$(ec "$org" | urls-extract | rg '(?:\.pdf$|^https?://dl.acm.org/doi/pdf/)')" ; then
    elif urls="$(ec "$org" | urls-extract | rget '^https://api.semanticscholar.org/arXiv:([^/]+)$' | head -n 1)" ; then
        urls="https://export.arxiv.org/pdf/${urls}.pdf"
    else
        urls="$(ec "$org" | rget '^\s*:pdf_urls:\s+(.*?)\s*$')" @TRET
    fi
    urls="$(ec "${urls}" | arxiv-exportify)" @TRET
    urls="$(ec "${urls}" | ugrep -i -v '^https?://ieeexplore.ieee.org/stamp/stamp.jsp')" @TRET
    urls=(${(@f)urls})

    local url
    url="${urls[1]}"
    if test -z "$url" ; then
        ecerr "$0: no suitable URLs found!"
    fi

    local name
    name="$(
    ec "$org" |
    org-link-extract-title |
    arxiv-exportify |
    ghead -n 1
    )" @TRET

    local dest
    dest="${name}"
    [[ "$dest" == *.pdf ]] || dest+=".pdf"
    dest="$(ec "$dest" | str2filename-ascii)"

    # reval-ec retry aa-gateway "$url" -o "${dest}" @RET
    # reval-ec retry wget "$url" -O "${dest}" @RET
    curlm_ns=y reval-ec retry curlm "$url" --output-dir "$dir" --create-dirs -o "${dest}" @RET
    dest="${dir}/${dest}"

    if bool "${tlg_p}" ; then
        local lock_id="$0"
        retry_sleep=15 assert lock-aquire-redis-retry "${lock_id}" $((30*60)) @RET
        setopt localtraps
        trap "" $exit_traps[@]
        {
            #: The subshell allows us to exit via C-c, which we have otherwise disabled via the trap above.
            (
                {
                    ec "$org" |
                        perl -ple 's/\@(?:toread\S*|tosee\S*|CR\b)\s?//g' |
                        org2tlg "$tlg_dest"
                } || {
                    ecerr "$0: failed to send the description to Telegram"
                    return 1
                }

                reval-env-ec tlg_dest="$tlg_dest" retry_sleep=10 retry-limited 10 tsendf-book "$dest"
            )
        } always {
            lock-release-redis "${lock_id}"
            trap - $exit_traps[@]
        }
    fi
}
aliasfn ssdl semantic-scholar-dl-from-org
@opts-setprefix semantic-scholar-dl-from-org ssdl

function semantic-scholar-get-and-dl {
    # if should-proxy-p ; then
    #     pxa89-local
    # fi

    local inargs=() ret=0
    in-or-args3 "$@" @RET

    local url
    for url in ${inargs[@]} ; do
        { semantic-scholar-to-org "$url" |
              semantic-scholar-dl-from-org } || {
            ret=$?
            ecerr "$0: URL failed: ${url}"
        }
    done

    return ${ret}
}
aliasfn jss semantic-scholar-get-and-dl
##
function semantic-scholar-id-get {
    in-or-args "$@" |
        perl -lne '(m|^(?::paperId:\s*)?([^/]{40})$| || m|(?:https://www.semanticscholar.org/paper/\|https://www.connectedpapers.com/main/)(?:(?:[^/]+)/)?([^/]{40})(?:/\|$)| ) && print $1' |
        cat-copy-if-tty
}

function connectedpapers-from-ss-id {
    local inargs
    inargs=("${(@f)$(in-or-args "$@" | semantic-scholar-id-get)}") @TRET

    for ss_id in ${(@)inargs} ; do
        ec "https://www.connectedpapers.com/main/${ss_id}"
    done |
        cat-copy-if-tty
}
##
