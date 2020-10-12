trs() {
    local i
    for i in "$@"
    do
        [[ -e "$i" ]] && {
            ec Trying to remove "$i"
            trash -- "$i"
        }
    done
}
aliasfn trs-empty empty-trash
