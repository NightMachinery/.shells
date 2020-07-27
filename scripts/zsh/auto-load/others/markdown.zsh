mdw() {
    pandoc "$@" | w3m -T text/html
}
mdv() {
    # mdview
    local readme="$(fd -e md|head -1)"
    md.py "${@:-$readme}" | bt
}
