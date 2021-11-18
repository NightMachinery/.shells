##
function arxiv-dl {
    local url="${1}" #: Example: =https://arxiv.org/abs/2109.02355=
    assert-args url @RET
    local dest="${arxiv_dl_o}"

    local url_pdf title
    if [[ "$url" =~ '/(?:abs|pdf)/([^/]+)/*$' ]] ; then
        url_pdf="https://arxiv.org/pdf/${match[1]}"

        if test -z "$dest" ; then
            title="$(url-title "$url" | str2filename)" @TRET
            dest="${title}.pdf"
        fi

        revaldbg aa-2dest "$url_pdf" "$dest" >&2 @TRET
        ec "$dest"
    else
        ecerr "$0: Bad URL $(gq "$url")"
        return 1
    fi
}
renog arxiv-dl

function arxiv2k {
    local url="${1}"
    assert-args url @RET

    local dled
    dled="$(arxiv-dl "$url")" @TRET
    k2pdf "$dled" @TRET
    local converted
    converted="${dled:r}_k2opt.pdf"
    assert test -e "$converted" @RET
    2ko "$converted" @TRET
}
renog arxiv2k
##
