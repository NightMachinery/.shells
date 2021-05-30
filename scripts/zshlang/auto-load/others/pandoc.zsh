function md2org() {
    @opts from markdown to org @ pandoc-convert "$@"
}
function org2md() {
    @opts from org to markdown @ pandoc-convert "$@"
}
function html2org() {
    local input="${1}"
    if test -z "$input" ; then
        input="$(gmktemp)"
        pbpaste-html > "$input" @RET
    fi

    @opts from html to org @ pandoc-convert "$input" "${@[2,-1]}"
}
function pandoc-convert() {
    local input="${1}" output="${2:--}" from="${pandoc_convert_from}" to="${pandoc_convert_to}"
    assert-args from to @RET

    if test -z "$input" ; then
        input="$(gmktemp)" @TRET
        assert pbpaste > "$input" @RET
    fi
    tmp_o="$(gmktemp)" @TRET


    pandoc --wrap=none --from "$from" --to "$to" "$input" -o "-" | pandoc-normalize-whitespace > "$tmp_o" @TRET

    if [[ "$output" != '-' ]] ; then
        assert command mv "$tmp_o" "$output" @RET
    else
        if isIReally ; then
            cat "$tmp_o" | pbcopy @TRET
        fi
        assert cat "$tmp_o" @RET
        assert silent trs-rm "$tmp_o" @RET
    fi
}
function pandoc-normalize-whitespace() {
    gsed 's/ / /g'
    # pandoc uses some bad whitespace that cannot be read by org-mode
}

## Deprecated as html2org does this already:
# function pop-html2org() {
#     reval-copy pandoc --from html --to org =(pbpaste) -o -
# }
# aliasfn p-h2o pop-html2org
##
