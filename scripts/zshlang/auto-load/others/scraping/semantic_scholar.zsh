##
function semantic-scholar-to-json-api {
    #: @docs https://api.semanticscholar.org/api-docs/graph#tag/Paper-Data/operation/get_graph_get_paper
    ##
    local paper_id="${1}" #: e.g., arXiv:1705.10311
    local corpus_id="${semantic_scholar_corpus_id}"

    if [[ "$paper_id" =~ '^https://api.semanticscholar.org/((?:arXiv|CorpusID):[^/]+)$' ]] ; then
        paper_id="${match[1]}"

        # typ paper_id
    elif [[ "$paper_id" =~ '^http' ]] && test -n "$corpus_id" ; then
        paper_id="CorpusID:${corpus_id}"
    fi

    if [[ "$paper_id" =~ '^(arXiv:[^/]+)v\d{1,2}$' ]] ; then
        paper_id="${match[1]}"
    fi

    local json
    json="$(revaldbg gurl "https://api.semanticscholar.org/graph/v1/paper/${paper_id}?fields=title,url,citationCount,influentialCitationCount,externalIds,abstract,venue,year,referenceCount,isOpenAccess,fieldsOfStudy,s2FieldsOfStudy,publicationTypes,publicationDate,journal,authors.name,authors.hIndex,authors.homepage,authors.affiliations,authors.citationCount,authors.paperCount,authors.aliases,authors.url,authors.externalIds")" @TRET
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
    json_api="$(semantic_scholar_corpus_id="$corpus_id" semantic-scholar-to-json-api "$url")" @TRET

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
    html="$(full-html2 "$url")" @TRET
    ec "$html" |
        selectors2json.py \
            title '[data-selenium-selector="paper-detail-title"]' '' \
            bibtex '.bibtex-citation' '' \
            doi '[data-selenium-selector="paper-doi"] .doi__link' 'attr:href' \
            corpusID '[data-selenium-selector="corpus-id"]' '' \
            date '[data-selenium-selector="paper-year"]' '' \
            authors_names 'meta[name="citation_author"]' 'attr:content' \
            journal_name '[data-heap-id="paper-meta-journal"]' '' \
            abstract 'meta[name="description"]' 'attr:content' \
            links '[data-selenium-selector="paper-link"]' 'attr:href' \
            pdf_urls 'meta[name="citation_pdf_url"]' 'attr:content' \
            topics '[data-selenium-selector="entity-name"]' '' \
            referenceCount '[data-heap-nav="references"]' '' \
            | jq '.'

    ##
    # abstract '[data-selenium-selector="abstract-text"]' '->org' \
        # authors '.author-list' '->org' \
        # '+1 author' appears in the authors list.
    # citationCount '[data-heap-nav="citing-papers"]' '' \
        ##
}

function semantic-scholar-to-org {
    local url="${1:-$(pbpaste)}"
    assert-args url @RET
    url="$(semantic-scholar-url-get "$url")" @TRET

    revaldbg semantic-scholar-to-json "$url" \
        | revaldbg semantic_scholar_json_to_org.lisp "$url" \
        | cat-copy-if-tty
}
##
function semantic-scholar-dl-from-org {
    local tlg_dest="${tlg_dest:-$tlg_ch_books}"

    local org
    org="$(cat-paste-if-tty)" @TRET

    local urls
    if ! urls="$(ec "$org" | urls-extract | rg '(?:\.pdf$|^https?://dl.acm.org/doi/pdf/)')" ; then
        if urls="$(ec "$org" | urls-extract | rget '^https://api.semanticscholar.org/arXiv:([^/]+)$' | head -n 1)" ; then
            urls="https://arxiv.org/pdf/${urls}.pdf"
        else
            urls="$(ec "$org" | rget '^\s*:pdf_urls:\s+(.*?)\s*$')" @TRET
        fi
    fi
    urls=(${(@f)urls})

    local url
    url="${urls[1]}"

    local name
    name="$(
    ec "$org" |
    org-link-extract-title |
    ghead -n 1
    )" @TRET

    local dest
    dest="${name}"
    [[ "$dest" == *.pdf ]] || dest+=".pdf"
    dest="$(ec "$dest" | str2filename-ascii)"

    reval-ec retry aa-gateway "$url" -o "${dest}" @RET

    ec "$org" |
        perl -ple 's/\@(?:toread\S+|CR\b)\s?//g' |
        org2tlg "$tlg_dest"

    reval-env-ec tlg_dest="$tlg_dest" retry_sleep=10 retry-limited 10 tsendf-book "$dest"
}
##
