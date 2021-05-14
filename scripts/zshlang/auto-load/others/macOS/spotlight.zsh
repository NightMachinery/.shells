function spotlight() {
    mdfind "$@" | fz --select-1 | tee >(pbcopy)
}

alias spt='spotlight -name'

function spot() {
    local file="$(spt ${@[-1]})"
    test -n "$file" && reval "${@[1,-2]}" "$file"
}
