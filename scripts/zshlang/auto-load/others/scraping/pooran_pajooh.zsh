##
function pooran-pajooh-update {
    assert-net @RET

    local url='https://www.pouran.net/product/%D8%A2%D8%B2%D9%85%D9%88%D9%86-%D9%87%D8%A7%DB%8C-%D8%AC%D8%A7%D9%85%D8%B9-%D8%A7%D8%B1%D8%B4%D8%AF-%D9%85%D9%87%D9%86%D8%AF%D8%B3%DB%8C-%DA%A9%D8%A7%D9%85%D9%BE%DB%8C%D9%88%D8%AA%D8%B1-%D9%87%D8%A7%D8%AF%DB%8C-%DB%8C%D9%88%D8%B3%D9%81%DB%8C/'
    local url_base='https://www.pouran.net'

    local orig_pwd="$PWD"
    {
        z data/pooran @TRET

        local html
        html="$(fhMode=curl full-html2 "$url")" @TRET
        html="$(ec "$html" | html-links-absolutify "$url_base")" @TRET
        # html="$(ec "$html" | htmlq '.boxMiddle')" @TRET
        assert not whitespace-p "$html" @RET
        ec "$html" | html2org | skipemptyin sponge arshad.org @TRET

        git-diff-report "$PWD"
    } always { cd "$orig_pwd" }
}
##
