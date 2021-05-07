function tumblr-post-dl() {
    local url="$1"
    assert-args url @RET

    local html
    html="$(full-html2 "$url")" @TRET

    local dl_urls=()
    if (( ${#dl_urls} == 0 )) ; then
        # needs API keys in ~/.gallery-dl.conf
        dl_urls=( ${(@f)"$($proxyenv gallery-dl --get-urls "$url")"} ) @TRET
    fi
    ##
    # dl_urls=( ${(@f)"$(ec $html | rget 'data-big-photo="([^"]+)"' )"} )

    # if (( ${#dl_urls} == 0 )) ; then
    #     dl_urls=( ${(@f)"$(ec $html | pup 'img.photo-post-photo attr{src}')"} ) @TRET
    # fi
    # if (( ${#dl_urls} == 0 )) ; then
    #     # GIFs:
    #     dl_urls=( ${(@f)"$(ec $html | pup 'div#pc img attr{src}')"} ) @TRET
    # fi
    ##
    if (( ${#dl_urls} == 0 )) ; then
        # works for GIFs and photo sets, too
        # the other  cases probably are not needed at all
        dl_urls=( ${(@f)"$(ec $html | pup 'meta[property="og:image"] attr{content}')"} ) @TRET
    fi


    if (( ${#dl_urls} == 0 )) ; then
        ecerr "$0: no links found"
    else
        $proxyenv aa-gateway -Z $dl_urls[@]
    fi
}
renog tumblr-post-dl
##
function tumblr2tlg() {
    local chat="${tumblr2tlg_chat:--1001408125632}"
    assert-args chat @RET

    pushf "$(uuidm)" @TRET
    {
        assert tumblr-post-dl "$@" @RET
        local f
        for f in *(DN.) ; do
            assert tsendf "$chat" "$f"
        done
    } always { popf }
}
##
