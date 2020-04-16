if (( $+commands[tag-ag] )); then
    export TAG_SEARCH_PROG=ag  # replace with rg for ripgrep
    export TAG_CMD_FMT_STRING='nvim -c "call cursor({{.LineNumber}}, {{.ColumnNumber}})" "{{.Filename}}"'
    agg() { command tag-ag "$@"; source ${TAG_ALIAS_FILE:-/tmp/tag_aliases} 2>/dev/null }
fi
##
alias agc='ec "${(F)commands}"|agC=0 agm'
##
function rg() {
    command rg --smart-case --colors "match:none" --colors "match:fg:255,120,0" --colors "match:bg:255,255,255" --colors "match:style:nobold" --engine auto -C ${agC:-1} "$@" # (use PCRE2 only if needed). --colors "match:bg:255,228,181" # This should've been on the personal list, but then it would not be loaded when needed by functions
}
agm() rg "$@" #alias agm='rg' #'ag -C "${agC:-1}" --nonumbers'

aga() {
    # agm "$@" "$NIGHTDIR"/**/*alias*(.)
    builtin alias|agm "$@"
}
ags() {
    agm "$@" ~/.zshenv ~/.zshrc "$NIGHTDIR"/**/*(.) ~/.bashrc ~/.profile ~/.bashrc ~/.bash_profile
}
agf() {
	ags "$*"'\s*\(\)'
}
agi() {
    doc ag internals of zsh
    agm "$@" ~/.oh-my-zsh/ $ANTIBODY_HOME
}
agcell() {
    agm -uuu --iglob '!.git' "$@" $cellar # --binary --hidden don't work with -C somehow, so we use -uuu :D
}
agrdry() {
    agm -F  -- "$from" "${@}"
}
function agr {
    doc 'usage: from=sth to=another agr [ag-args]'
    comment -l --files-with-matches

    ag -0 -l --literal -- "$from" "${@}" | pre-files "$from" "$to"
}
