if (( $+commands[tag-ag] )); then
    export TAG_SEARCH_PROG=ag  # replace with rg for ripgrep
    export TAG_CMD_FMT_STRING='nvim -c "call cursor({{.LineNumber}}, {{.ColumnNumber}})" "{{.Filename}}"'
    agg() { command tag-ag "$@"; source ${TAG_ALIAS_FILE:-/tmp/tag_aliases} 2>/dev/null }
fi
aga() {
    agm "$@" "$NIGHTDIR"/**/alias*(.)
}
agf() {
    agm "$@" "$NIGHTDIR"/**/functions*(.)
}
ags() {
    agm "$@" ~/.zshenv ~/.zshrc "$NIGHTDIR"/**/*(.)
}
function agr {
    doc 'usage: from=sth to=another agr [ag-args]'
    comment -l --files-with-matches

    ag -0 -l --literal -- "$from" "${@}" | pre-files "$from" "$to"
}
