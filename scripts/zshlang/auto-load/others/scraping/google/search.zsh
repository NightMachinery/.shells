##
function google-search-urls {
    xidel "https://www.google.com/search?q=$(html-encode "$*")" --extract "//a/extract(@href, 'url[?]q=([^&]+)&', 1)[. != '']"
}
##
