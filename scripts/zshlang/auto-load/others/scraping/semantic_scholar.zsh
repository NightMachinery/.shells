##
function semantic-scholar-to-json {
    # @alt https://www.semanticscholar.org/product/api
    ##
    local url="${1:-$(pbpaste)}"
    assert-args url @RET

    eval-memoi full-html2 "$url" \
        | selectors2json.py \
        title '[data-selenium-selector="paper-detail-title"]' '' \
        bibtex '.bibtex-citation' '' \
        doi '[data-selenium-selector="paper-doi"] .doi__link' 'attr:href' \
        corpusID '[data-selenium-selector="corpus-id"]' '' \
        date '[data-selenium-selector="paper-year"]' '' \
        authors 'meta[name="citation_author"]' 'attr:content' \
        journal '[data-heap-id="paper-meta-journal"]' '' \
        abstract 'meta[name="description"]' 'attr:content' \
        links '[data-selenium-selector="paper-link"]' 'attr:href' \
        topics '[data-selenium-selector="entity-name"]' '' \
        citations '[data-heap-nav="citing-papers"]' '' \
        references '[data-heap-nav="references"]' '' \
        | jq '.'

    ##
    # abstract '[data-selenium-selector="abstract-text"]' '->org' \
    # authors '.author-list' '->org' \
    # '+1 author' appears in the authors list.
    ##
}

function semantic-scholar-to-org {
    local url="${1:-$(pbpaste)}"
    assert-args url @RET

    semantic-scholar-to-json "$url" \
        | semantic_scholar_json_to_org.lisp "$url" \
        | cat-copy-if-tty
}
##
