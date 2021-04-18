## Google Docs
function gdoc-epub() {
    local id url="$1"
    [[ "$url" =~ '/document(?:/.*)?/d/([^/]*)/' ]] && id="$match[1]" || { ecerr "couldn't find gdoc id in the url $url" ; return 1 }
    gdoc-epub-byid "$id"
}
function gdoc-epub-byid() {
    local id="$1"
    aab 'https://docs.google.com/document/u/0/export?format=epub&id='"$id"
}
