mdview2() {
    pandoc "$@" | w3m -T text/html
}
mdview() {
    local readme="$(fd -e md|head -1)"
    md.py "${@:-$readme}" | bt
}
aliasfn mdv mdview
aliasfn mdv2 mdview2
