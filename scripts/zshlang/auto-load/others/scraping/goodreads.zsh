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
    local url="$1"
    assert-args url @RET

    eval-memoi 'full-html2' "$url" \
        | selectors2json.py \
        description '#descriptionContainer span[style="display:none"]' \
        bookFormat '[itemprop="bookFormat"]' \
        pageCount '[itemprop="numberOfPages"]' \
        publicationDetails '#details > div.row:nth-child(2)' \
        title '#bookDataBox .clearFloats:nth-child(1) .infoBoxRowTitle+.infoBoxRowItem' \
        isbn '[itemprop="isbn"]' \
        language '[itemprop="inLanguage"]' \
        series '#bookDataBox a[href^="/series/"]' \
        genres '.actionLinkLite.bookPageGenreLink' \
        | sd '\[\[/' '[[https://www.goodreads.com/' \
        | jq '.'
}

function goodreads-url-to-org {
    local url="$1"
    assert-args url @RET

    goodreads-url-to-json "$url" \
        | goodreads_url_json_to_org.lisp "$url" \
        | cat-copy-if-tty
}
##
