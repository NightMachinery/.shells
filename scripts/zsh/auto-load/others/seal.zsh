seal() {
    doc Use with 'uns' to store and retrieve one-liners
    doc Use exor to remove seals.
    ec "$@" >> "$attic"
}
uns() {
    doc unseal
    local l="$(cat "$attic" | fz -q "${@:-}")"
    test -n "$l" && {
        { [[ "$l" != (@|\#)* ]] && test -z "$un_p" } && print -z -- "$l" || ec "$l"
        ec "$l"|pbcopy
        return 0
    } }
exor() {
    doc exorcize seals :D
    local sels="$(un_p=y uns "$@")"
    test -n "$sels" && {
        local i
        for i in "${(@f)sels}"
        do
            ec Exorcizing 🏺 "$i"
            sd --string-mode "$i
" '' "$attic"
        done
    }
}
