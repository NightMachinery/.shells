##
function sanjesh-phd-update {
    #: =sanjesh-arshad-update=
    ##
    assert-net @RET

    # local url='http://sanjesh.org/group.aspx?gid=2'
    local url='https://www.sanjesh.org/fa-IR/sanjesh/4929/page/%D8%AF%DA%A9%D8%AA%D8%B1%D8%A7%DB%8C-%D8%AA%D8%AE%D8%B5%D8%B5%DB%8C'
    local url_base='https://sanjesh.org'

    local orig_pwd="$PWD"
    {
        z data/sanjesh @TRET

        local html
        html="$(fhMode=curl full-html2 "$url")" @TRET
        html="$(ec "$html" | html-links-absolutify "$url_base")" @TRET
        html="$(ec "$html" | htmlq '.dp-module-content')" @TRET
        assert not whitespace-p "$html" @RET
        ec "$html" | html2org | skipemptyin sponge phd.org @TRET

        git-diff-report "$PWD"
    } always { cd "$orig_pwd" }
}
##
