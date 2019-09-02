if (( $+commands[tag-ag] )); then
    export TAG_SEARCH_PROG=ag  # replace with rg for ripgrep
    export TAG_CMD_FMT_STRING='nvim -c "call cursor({{.LineNumber}}, {{.ColumnNumber}})" "{{.Filename}}"'
    agg() { command tag-ag "$@"; source ${TAG_ALIAS_FILE:-/tmp/tag_aliases} 2>/dev/null }
fi
##
alias agc='ec "${(F)commands}"|agm'
##
aga() {
    # agm "$@" "$NIGHTDIR"/**/*alias*(.)
    builtin alias|agm "$@"
}
agf() {
    agm "$@" "$NIGHTDIR"/**/functions*(.)
}
ags() {
    agm "$@" ~/.zshenv ~/.zshrc "$NIGHTDIR"/**/*(.)
}
agi() {
    doc ag internals of zsh
    agm "$@" ~/.oh-my-zsh/ $ANTIBODY_HOME
}
agcell() {
    agm -uuu --iglob '!.git' "$@" $cellar # --binary --hidden don't work with -C somehow, so we use -uuu :D
}
function agr {
    doc 'usage: from=sth to=another agr [ag-args]'
    comment -l --files-with-matches

    ag -0 -l --literal -- "$from" "${@}" | pre-files "$from" "$to"
}
