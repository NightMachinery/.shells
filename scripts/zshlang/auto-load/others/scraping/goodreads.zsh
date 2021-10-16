##
function csv2json() {
    local f="$1"
    assert-args f @RET

    mlr --icsv --ojson --jlistwrap cat "$f" | jq '.'
    # --jlistwrap Wrap JSON output in outermost [ ].
}
##
function goodreads-export() {
    ##
    # this request makes goodreads start generating the export file, then the browser starts making requests to download the file, and retries till the file is is ready
    # this request will also delete the previously generated csv file at https://www.goodreads.com/review_porter/export/13353762/goodreads_export.csv
    ##
    local tmp
    tmp="$(gmktemp --suffix .html)" @TRET
    ectrace_notrace=y assert goodreads_export.js "$(cookies 'https://www.goodreads.com' | @opts d [ '.goodreads.com' 'www.goodreads.com' ] @ cookies2json)"  > $tmp || {
        ectrace "$0: goodreads_export.js failed; Its HTML output is available at $(gq "$tmp")"
        return 1
    }
    ##
    # the curl req doesn't seem to work, use ~/scripts/javascript/scraping/goodreads_export.js instead
    ##
}
function goodreads-export-dl() {
    local o="$1"
    local dir="${${o:h}:-.}"
    assert-args o @RET
    assert test -d "$dir" @RET

    assert goodreads-export @RET
    retry_sleep=15 assert retry-limited 100 aacookies --dir $dir -o "${o:t}" https://www.goodreads.com/review_porter/export/13353762/goodreads_export.csv @RET
}
function goodreads-export-backup() {
    local o
    # o="${nightNotes}/private/backups/Goodreads/goodreads_export_$(datej | str2filename).csv"
    o="${nightNotes}/private/backups/Goodreads/goodreads_export.csv"

    goodreads-export-dl $o
}
##
function goodreads-url-to-json {
    : "the output of this function needs further processing by goodreads_url_json_to_org.lisp; export JSON from that script if you want to use it elsewhere."

    local url="${1:-$(pbpaste)}"
    assert-args url @RET

    eval-memoi 'full-html2' "$url" \
        | html-links-absolutify 'https://www.goodreads.com' \
        | selectors2json.py \
        description '#descriptionContainer span[style="display:none"]' '->org' \
        bookFormat '[itemprop="bookFormat"]' '' \
        pageCount '[itemprop="numberOfPages"]' '' \
        publicationDetails '#details > div.row:nth-child(2)' '->org' \
        title '#bookTitle' '' \
        authors '#bookAuthors' '->org' \
        rating '[itemprop="ratingValue"]' '' \
        ratingCount '[itemprop="ratingCount"]' 'attr:content' \
        reviewCount '[itemprop="reviewCount"]' 'attr:content' \
        isbn '[itemprop="isbn"]' '' \
        language '[itemprop="inLanguage"]' '' \
        series '#bookDataBox a[href^="/series/"]' '' \
        genres '.actionLinkLite.bookPageGenreLink' '->org' \
        | jq '.'

    ## old selectors:
    # title '#bookDataBox .clearFloats:nth-child(1) .infoBoxRowTitle+.infoBoxRowItem' \
    ## deceptive selectors:
    # series '#bookSeries'
    ##
}

function goodreads-url-to-org {
    local url="${1:-$(pbpaste)}"
    assert-args url @RET

    goodreads-url-to-json "$url" \
        | goodreads_url_json_to_org.lisp "$url" \
        | cat-copy-if-tty
}
##
function goodreads-author-works-get {
    assert-net @RET

    local url="$1"
    assert-args url @RET

    local url_base='https://www.goodreads.com'

    local html
    html="$(full-html2 "$url")" @TRET
    html="$(ec $html | html-links-absolutify "$url_base")" @TRET
    html="$(ec $html | htmlq 'table.tableList .bookTitle')" @TRET
    assert not whitespace-p "$html" @RET

    ##
    # ec $html | html2org | double-newlines
    ##
    local urls
    urls="$(ec $html | htmlq '*' -a href)" @TRET
    ec "$urls"
    ##
}
##
function goodreads-url-to-tlg {
    local url="$1" dest="${goodreads_url_to_tlg_dest:-${water}}"
    assert-args url dest @RET

    local org
    org="$(goodreads-url-to-org "$url")" @TRET

    ##
    # ec "$org" | org2tlg "$dest"
    ##
    tsend -- "$dest" "$org"
    ##
}
##
