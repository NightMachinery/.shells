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
    fi

    local tmp
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
        ecgray "$0: curl-ing the API failed with $?"$'\n'"URL: ${url}"
        return 1
    }
    #: =tldr= sometimes isn't available for the newer papers, and we aren't currently using it, so I have omitted it from the request above. (It will trow an error if tldr is not available.)

    ec "$json" |
        jq .
}

function h-semantic-scholar-page-url {
    #: Resolve external IDs through Semantic Scholar's own redirect, without search.
    local input="${1}" url resolved
    local page_pattern='^https://(?:www\.)?semanticscholar\.org/paper/(?:[^/?#]+/)?[a-fA-F0-9]{40}(?:[?#].*)?$'
    url="$(semantic-scholar-url-get "${input}")" @RET
    if [[ "${url}" =~ "${page_pattern}" ]] ; then
        ec "${url%%\#*}"
        return 0
    fi
    if [[ "${url}" != https://api.semanticscholar.org/* ]] ; then
        ecerr "$0: cannot resolve a Semantic Scholar paper from ${input}"
        return 1
    fi
    #: Do not require the destination HTML to be readable: its redirect URL is
    #: useful even when the final page presents a browser-verification screen.
    resolved="$(command curl --head --silent --show-error --location --retry 2 --retry-delay 1 --max-time 30 \
        --output /dev/null --write-out '%{url_effective}' "${url}")" @RET
    if [[ "${resolved}" =~ "${page_pattern}" ]] ; then
        ec "${resolved%%\#*}"
    else
        ecerr "$0: Semantic Scholar did not resolve ${input} to a public paper page"
        return 1
    fi
}

function semantic-scholar-to-json {
    local api_p="${semantic_scholar_api_p:-${SemanticScholar_API_key:+y}}"
    local scraper="${semantic_scholar_scraper:-auto}"
    local input="${1:-$(pbpaste)}" url json_api json
    assert-args input @RET
    case "${scraper}" in
        auto|deepseek|page-data|full-html2) ;;
        *) ecerr "$0: semantic_scholar_scraper must be auto, deepseek, page-data, or full-html2"; return 1 ;;
    esac
    if bool "${api_p}" ; then
        url="$(semantic-scholar-url-get "${input}")" @RET
        if json_api="$(semantic-scholar-to-json-api "${url}")" &&
                json="$(ec "${json_api}" |
                    command jq --exit-status 'select(type == "object" and (.title | type == "string" and length > 0)) | with_entries(select(.value != null and .value != "" and .value != []))' |
                    json-listify-first-level | json-stringify-atoms |
                    command python3 "${NIGHTDIR}/python/orgmode/semantic_scholar_html_to_json.py" --validate "${url}")" ; then
            ec "${json}"
            return 0
        fi
        ecgray "$0: API failed; using ${scraper} scraper"
    fi
    semantic-scholar-to-json-scraping "${input}"
}

function h-semantic-scholar-scrape-deepseek {
    local url="${1}" page
    page="$(deepseek-fetch-json "${url}")" @RET
    ec "${page}" | command python3 "${NIGHTDIR}/python/orgmode/semantic_scholar_html_to_json.py" --fetch-json "${url}"
}

function h-semantic-scholar-scrape-page-data {
    #: This public endpoint supplies the main paper data to the website.
    local url="${1}" paper_id page
    paper_id="$(semantic-scholar-id-get "${url}")" @RET
    [[ "${paper_id}" =~ '^[a-fA-F0-9]{40}$' ]] || return 1
    page="$(command curl --fail --silent --show-error --retry 2 --retry-delay 1 --max-time 30 \
        "https://www.semanticscholar.org/api/1/paper/${paper_id}")" @RET
    ec "${page}" | command python3 "${NIGHTDIR}/python/orgmode/semantic_scholar_html_to_json.py" --page-data "${url}"
}

function h-semantic-scholar-scrape-html {
    local url="${1}" html
    html="$(fhMode="${fhMode:-curlfull_s5}" full-html2 "${url}")" @RET
    ec "${html}" | command python3 "${NIGHTDIR}/python/orgmode/semantic_scholar_html_to_json.py" "${url}"
}

function semantic-scholar-to-json-scraping {
    local scraper="${semantic_scholar_scraper:-auto}"
    local input="${1:-$(pbpaste)}" url json backend
    local backends=()
    case "${scraper}" in
        auto|deepseek|page-data|full-html2) ;;
        *) ecerr "$0: semantic_scholar_scraper must be auto, deepseek, page-data, or full-html2"; return 1 ;;
    esac
    ensure-cmd python3 @RET
    url="$(h-semantic-scholar-page-url "${input}")" @RET
    case "${scraper}" in
        deepseek) backends=(deepseek page-data) ;;
        full-html2) backends=(html page-data) ;;
        page-data) backends=(page-data) ;;
        auto)
            if test -n "${deepseek_api_key}" ; then
                backends+=(deepseek)
            fi
            backends+=(page-data html)
            ;;
    esac
    for backend in "${backends[@]}" ; do
        if json="$("h-semantic-scholar-scrape-${backend}" "${url}")" ; then
            ec "${json}"
            return 0
        fi
        ecgray "$0: ${backend} did not provide complete paper metadata"
    done
    ecerr "$0: could not recover the paper's title, authors, and full abstract from Semantic Scholar"
    return 1
}

function semantic-scholar-to-org {
    #: This function should probably be called only with a single arg ... I doubt combining its output for mutiple URLs will be good.
    ##
    local inargs
    in-or-args3 "$@" @RET

    local url
    for url in ${inargs[@]} ; do
        local json
        json="$(revaldbg semantic-scholar-to-json "$url")" @TRET

        local paper_url
        paper_url="$(ec "${json}" | command jq --raw-output '.url[0] // empty')" @RET
        if test -z "${paper_url}" ; then
            paper_url="$(h-semantic-scholar-page-url "${url}")" @RET
        fi
        ec "$json" |
            revaldbg semantic_scholar_json_to_org.lisp "$paper_url"
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
    #: [agfi:h-arxiv-pdf-dl] is a no-op for non-arXiv URLs, so it needs no test
    #: here: arXiv papers get the =arxiv.org= fallback, everything else gets a
    #: single URL. It replaces curl's =--create-dirs=, hence the =mkdir-m=.
    mkdir-m "$dir" @RET
    dest="${dir}/${dest}"
    h-arxiv-pdf-dl "$url" "${dest}" @RET

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
