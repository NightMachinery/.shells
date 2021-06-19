function epub2org() {
    @opts from epub to org trim_extra n @ pandoc-convert "$@"
}

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
    local input="${1}" output="${2:--}" from="${pandoc_convert_from}" to="${pandoc_convert_to}" trim_extra="${pandoc_convert_trim_extra-y}"
    assert-args from to @RET

    if test -z "$input" ; then
        input="$(gmktemp)" @TRET
        assert pbpaste > "$input" @RET
    fi
    tmp_o="$(gmktemp)" @TRET


    pandoc --wrap=none --from "$from" --to "$to" "$input" -o "-" | {
        if [[ "$to" == org ]] ; then
            pandoc-normalize-whitespace | {
                if bool $trim_extra ; then
                    perl -0777 -pe 's/(?:\n|\A)\h*(?:(?::PROPERTIES:(?:.|\n)*?:END:)|(?:<<.*?>>))\h*//g'
                    # [[nightNotes:private/playgrounds/pandoc.zsh::perl -0777 -pe][perl]]
                else
                    cat
                fi
            }
        else
            cat
        fi
    } > "$tmp_o" @TRET # noflycheck

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
    #
    # https://superuser.com/questions/517847/use-sed-to-replace-nbsp-160-hex-00a0-octal-240-non-breaking-space
}

## Deprecated as html2org does this already:
# function pop-html2org() {
#     reval-copy pandoc --from html --to org =(pbpaste) -o -
# }
# aliasfn p-h2o pop-html2org
##
