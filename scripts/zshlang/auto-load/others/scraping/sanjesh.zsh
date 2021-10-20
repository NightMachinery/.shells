##
function sanjesh-arshad-update {
    assert-net @RET

    local url='http://sanjesh.org/group.aspx?gid=2'
    local url_base='http://sanjesh.org'

    local orig_pwd="$PWD"
    {
        z data/sanjesh @TRET

        local html
        html="$(fhMode=curl full-html2 "$url")" @TRET
        html="$(ec "$html" | html-links-absolutify "$url_base")" @TRET
        html="$(ec "$html" | htmlq '.boxMiddle')" @TRET
        assert not whitespace-p "$html" @RET
        ec "$html" | html2org | skipemptyin sponge arshad.org @TRET

        git-diff-report "$PWD"
    } always { cd "$orig_pwd" }
}
##
