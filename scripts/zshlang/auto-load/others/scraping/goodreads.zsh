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
    local o="${nightNotes}/backups/Goodreads/goodreads_export_$(datej | str2filename).csv"

    goodreads-export-dl $o
}
##
