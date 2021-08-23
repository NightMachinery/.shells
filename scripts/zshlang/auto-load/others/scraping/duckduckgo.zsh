function ddg-img-thumb-urls() {
    local query="$*" pages="${ddg_img_pages:-0}" # it seems DDG doesn't let us see more than pages=8; Probably impossible to circumvent as the real users are similarly limited
    assert-args query @RET

    query="$(url-encode "$query")" @TRET
    local url="https://duckduckgo.com/?q=$query&t=hy&va=g&iax=images&ia=images"

    cfScrollDown="$pages" memoi_key="$pages" revaldbg eval-memoi withchrome getlinks-img "$url" | rget '^https://external-content\.duckduckgo\.com/iu/\?u=(http.*)' | url-decode
    # without Chrome, the page would be almost empty
}
@opts-setprefix ddg-img-thumb-urls ddg-img

function ddg-img-thumb-dl() {
    local query="$*"

    ddg-img-thumb-urls "$query" | {
        # @note some of URLs we get from that function can be dead
        inargsf @opts halt soon,fail=30% jobs 50 @ para -k aa_out_name='.jpg' aa-hash-name ::: @RET
        # aa_out_name='.jpg' inargsf re 'aa-hash-name'
    } @RET # @noflycheck
}
@opts-setprefix ddg-img-thumb-dl ddg-img
alias jimg-ddg='ddg-img-thumb-urls'
@opts-setprefix jimg-ddg ddg-img
##
