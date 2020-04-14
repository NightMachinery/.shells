rndint() {
    mdoc "Usage: $0 <from> <to> <how-many>
Doesn't output duplicates. (Outputs permutations.)
The from-to range is inclusive." MAGIC
    shuf -i "$1-$2" -n "${3:-1}"
}
rndarr() {
    local i="$(rndint 1 ${#@})"
    ec "${@[$i]}"
}
