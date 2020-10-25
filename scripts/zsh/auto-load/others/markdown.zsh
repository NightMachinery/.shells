mdview2() {
    pandoc "$@" | w3m -T text/html
}
mdview() {
    local readme="$(fd -e md|head -1)"
    md.py "${@:-$readme}" | bt
}
aliasfn mdv mdcat # --paginate # paginate breaks image support
aliasfn mdv1 mdview
aliasfn mdv2 mdview2
aliasfn mdv3 LESS= mdless
