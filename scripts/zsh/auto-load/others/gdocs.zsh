## Google Docs
function gdocs-epub() {
    local id url="$1"
    [[ "$url" =~ '/document/d/([^/]*)/' ]] && id="$match[1]" || { ecerr "couldn't find gdocs id in the url $url" ; return 1 }
    gdocs-epub-byid "$id"
}
function gdocs-epub-byid() {
    local id="$1"
    aab 'https://docs.google.com/document/u/0/export?format=epub&id='"$id"
}
