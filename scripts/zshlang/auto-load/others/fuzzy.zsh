function fuzzy-choose() {
    : "Usage: arrN <choice> ... | fuzzy-choose query # returns candidates in order"
    fuzzyChoose.js "$*" | jqm '.[] | .[1]'
}
##
function fzinw() {
    doc 'fz in words: allows you to select the part of the output you need from a command. (alt: smenu?)'
    local q="$(fz-createquery "$@")"

    local res
    res="$(iaIFS=$' \t\n\C-@'"'"'(){}"[]' inargss arrN | fzp "$q")" || return 1
    ec-copy "$res"
}
