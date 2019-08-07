seal() {
    doc Use with 'uns' to store and retrieve one-liners
    doc Use exor to remove seals.
    local n=''
    ! test -e "$attic" || n=$'\0'
    print -r -n -- "$n$@"$'\n' >> "$attic"
}
uns() {
    doc unseal
    doc in: un_fz un_p
    re 'ecdbg un_fz:'  "$un_fz[@]"
    local l="$(cat "$attic" | fz $un_fz[@] --read0 -q "${@:-}")"
    test -n "$l" && {
        { [[ "$l" != (@|\#)* ]] && test -z "$un_p" } && print -z -- "$l" || ec "$l"
        ec "$l"|pbcopy
        return 0
    } }
exor() {
    doc exorcize seals :D
    sels="$(un_p=y un_fz=( --print0 ) uns "$@")"
    test -n "$sels" && {
        local i items
        ecdbg sels: "$sels"
        ecdbg items: "$items[@]"
        for i in "${(@0)sels}"
        do
            test -n "$i" || continue
            ec Exorcizing 🏺
            cat -v <<<"$i"
            # sd --string-mode "$i" '' "$attic" #--flags m 
            FROM="$i" perl -007 -pi -e 's/(\0|\A)\Q$ENV{FROM}\E(?<sep>\0|\Z)/$+{sep}/gm' "$attic"
        done
        # perl -pi -e 's/\0\0+/\0/g' "$attic"
        perl -0 -pi -e 's/\A\0//' "$attic"
    }
}
