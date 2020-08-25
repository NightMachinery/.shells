function org2md() {
    local input="${1}" output="${2:--}"
    if test -z "$input" ; then
        input="$(mktemp)"
        pbpaste > "$input" # this file leaks, but who cares.
    fi
    pandoc --from org --to markdown "$input" -o "$output"
}
