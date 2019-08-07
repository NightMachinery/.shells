## vars
test -z "$attic_dir" && attic_dir="$cellar/attic/"
test -z "$attic" && attic="$attic_dir/.darkattic"
test -z "$attic_todo" && attic_todo="$attic_dir/.attic_todo"

## aliases
# todo
alias todo='attic="$attic_todo" un_p=y seal'
alias todos='attic="$attic_todo" un_p=y unseal'
alias todone='attic="$attic_todo" un_p=y exor'
alias todo-import='attic="$attic_todo" un_p=y seal-import'
alias td=todo
alias tds='cat "$attic_todo"'
# alias ts=todos #CONFLICTING_NAME
alias tn=todone
alias tdi=todo-import
## core
seal() {
    doc Use with 'uns' to store and retrieve one-liners
    doc Use exor to remove seals.
    local n=''
    ! test -e "$attic" || n=$'\0'
    print -r -n -- "$n$(in-or-args "$@")"$'\n' >> "$attic"
}
alias uns=unseal
unseal() {
    doc unseal
    doc in: un_fz un_p
    re 'ecdbg un_fz:'  "$un_fz[@]"
    local l="$(cat "$attic" | fz $un_fz[@] --read0 --tac --no-sort -q "${@:-}")"
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
            FROM="$i" perl -0777 -pi -e 's/(\0|\A)\Q$ENV{FROM}\E(?<sep>\0|\Z)/$+{sep}/gm' "$attic"
            comment "This actually doesn't do the 'g' part completely. It removes some occurences, but not all."
        done
        # perl -pi -e 's/\0\0+/\0/g' "$attic"
        perl -0 -pi -e 's/\A\0//' "$attic"
    }
}
alias seali=seal-import
seal-import() {
    local inp="$(in-or-args "$@")"
    re seal "${(@f)inp}"
}

