function org2md() {
    local input="${1}" output="${2:--}"
    if test -z "$input" ; then
        input="$(mktemp)"
        pbpaste > "$input" # this file leaks, but who cares.
    fi
    pandoc --from org --to markdown "$input" -o "$output"
}
##
function pop-html2org() {
    reval-copy pandoc --from html --to org =(pbpaste) -o -
}
aliasfn p-h2o pop-html2org
