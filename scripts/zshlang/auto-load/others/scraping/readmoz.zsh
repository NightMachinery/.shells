##
function readmoz() {
    magic mdoc "[rmS= rmHtml= readmoz_nosummary=] $0 <url>
Outputs a summary of the URL and a cleaned HTML of the webpage to stdout. Set rmS to only print the summary." ; mret

    local url="$1"
    local summaryMode="$rmS"
    local noSummaryMode="${readmoz_nosummary:-$readmoz_ns}"

    local html
    html="${rmHtml:-$(full-html2 "$url")}" || {
        ecerr "${0}: Could not download $url; aborting."
        return 1
    }
    if ! ishtml-file =(ec "$html") ; then
        ecerr "$url is not an HTML file. Aborting."
        return 0 # this is not exactly an error. Returning 1 might cause useless retries.
    fi

    local cleanedhtml
    cleanedhtml="$(<<<"$html" readability "$url")" @TRET

    cleanedhtml="$(<<<"$cleanedhtml" html-links-absolutify "$url")" @TRET

    if ! bool "$noSummaryMode" ; then
        local prehtml="$(url2html "$url")"
        ec "$prehtml <hr> "
        # <p> --- </p>
    fi
    test -n "$summaryMode" || ec "$cleanedhtml"
}
noglobfn readmoz

function readmoz-nosanitize {
    readability_sanitize_disabled=y readmoz "$@"
}
noglobfn readmoz-nosanitize

function readmozsum() {
    : "Use url2html instead? No advtanges to this."
    rmS=y readmoz "$@"
}
noglobfn readmozsum

function readmozsum-file() {
    rmS=y readmoz-file "$@"
}
noglobfn readmozsum-file

function readmoz-file() {
    magic mdoc "$0 <file> [<url>]"
    local file="$1" url="${2:-https://${$(basename "$file"):-empty}.google.com}"
    local rmHtml="$(< "$file")"
    readmoz "$url"
}
noglobfn readmoz-file

function readmoz-mdold() {
    arcMode=oldest transformer to-archive-is re readmoz-md "$@"
}
noglobfn readmoz-mdold

function readmoz-mdarc() {
    fhMode=curlfullshort transformer to-archive-is re readmoz-md "$@"
}
noglobfn readmoz-mdarc

##
# function readmoz-md-old() {
#     local url="$1"
#     local format=".${2:-md}"

#     local md="$(gmktemp --suffix "$format")"
#     # <() would not work with: readmoz-md https://github.com/google/python-fire/blob/master/docs/guide.md | cat
#     # zsh sure is buggy :|
#     pandoc -s -f html-native_divs =(readmoz "$url") -o $md
#     < $md
#     \rm $md
# }
##
function readmoz-md {
    local url="$1"
    local format="${readmoz_md_to:-markdown}"
    assert-args url @RET

    local tmp
    tmp="$(gmktemp)" @TRET
    {
        readmoz "$url" > $tmp @TRET

        @opts from html-native_divs to "$format" @ pandoc-convert "$tmp" "${@[2,-1]}"
    } always { silent trs-rm "$tmp" }
}
noglobfn readmoz-md

function readmoz-org {
    @opts to org @ readmoz-md "$1"
}
noglobfn readmoz-org

function readmoz-md2() {
    readmoz "$1" | html2text "${@:2}" # --ignore-links
}
noglobfn readmoz-md2

function readmoz-org() {
    readmoz "$1" | html2org "${@:2}"
}
noglobfn readmoz-org

function readmoz-txt() {
    local opts=( "${@:2}" )
    test -n "$opts[*]" || opts=(--ignoreHref --ignoreImage --wordwrap=false --uppercaseHeadings=false --tables=true)
    readmoz "$1" | html-to-text "${opts[@]}" # returnDomByDefault
}
noglobfn readmoz-txt
##
