##
function git-diff-report {
    local root
    root="$(gitroot)" @TRET
    root="${root:t}"

    local diff
    diff="$(git-diff)" @TRET
    if whitespace-p "$diff" ; then
        ecgray "$0: Empty diff: $(gquote-sq "$PWD")"
        return 0
    fi
    local diff_colored
    diff_colored="$(ec "$diff" | diff-colorer)" @TRET
    local diff_html
    diff_html="$(ec "$diff_colored" | ansi2html)" @TRET
    local tmp
    tmp="$(mktemp-exact "${root}_diff.html")" @TRET
    {
        ec "$diff_html" > "$tmp" @TRET
        @opts opts [ --file="$tmp" ] @ tnotif "$0: ${root}" @RET
    } always { silent trs-rm "$tmp" }

    git_commitmsg_ask=n gsync
}
##
