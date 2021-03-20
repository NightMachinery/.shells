function md2org() {
    @opts from markdown to org @ pandoc-convert "$@"
}
function org2md() {
    @opts from org to markdown @ pandoc-convert "$@"
}
function html2org() {
    @opts from html to org @ pandoc-convert "$@"
}
function pandoc-convert() {
    local input="${1}" output="${2:--}" from="${pandoc_convert_from}" to="${pandoc_convert_to}"
    ensure-args from to @MRET
    if test -z "$input" ; then
        input="$(gmktemp)"
        pbpaste > "$input" @RET
    fi

    reval-copy pandoc --from "$from" --to "$to" "$input" -o "$output" @RET
    if [[ "$output" != '-' ]] ; then
        cat "$output" | pbcopy || true
    fi
}
## Deprecated as html2org does this already:
# function pop-html2org() {
#     reval-copy pandoc --from html --to org =(pbpaste) -o -
# }
# aliasfn p-h2o pop-html2org
##
