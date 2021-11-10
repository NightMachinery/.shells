##
function tlrl-txt {
    local url="$1"
    assert-args url @RET
    local title
    title="${tlrl_title:-$(url-tail "$url")}" @TRET

    t2e "$title" =(gurl "$url")
}
@opts-setprefix tlrl-txt tlrl
renog tlrl-txt
alias tltxt='tlrl-txt'
##
