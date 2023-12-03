##
function w2e-lw-raw() {
    w2e-curl "$1" "${(@f)$(lw2gw "${@:2}")}"
    # transformer lw2gw "w2e-curl $(gq "$1")" "${@:2}"
    # transformer urlfinalg "transformer lw2gw w2e-curl $(gq "$1")" "${@:2}"
}

function tllw {
    #: We can't use an alias because tlrl-ng won't get the correct URLs then.
    tll "${(@f)$(lw2gw "${@}")}"
}
noglobfn tllw

function lw2gw() {
    local inargs
    inargs="$(in-or-args "$@")" @RET

    local from='(lesswrong\.com|alignmentforum\.org)' to='greaterwrong.com'

    ec "$inargs" | url-clean | sd "$from" "$to" | cat-copy-if-tty
}
noglobfn lw2gw
##
function lwseq-get {
    lw2gw "$@" | inargsf getlinks-c | command rg -F lesswrong.com/ | inargsf lw2gw
}

function lwseq {
    mdoc "Usage: [tl options] URL ...
    Creates an ebook out of the sequences specified." MAGIC

    local opts
    zparseopts -A opts -K -E -D -M -verbose+=v v+ -prefix-title:=p p: -engine:=e e: -outputdir:=o o:

     lwseq-get "$@" | inargsf tlrl-ng -e "${opts[-e]:-w2e-curl}" -p "${opts[-p]}" -o "${opts[-o]:-./}"
}
noglobfn lwseq
##
function p-lw-urls {
    local urls
    urls="$(p-getlinks)" @TRET
    assert not isSpace urls @RET

    if ! { ec $urls | rg -q 'greaterwrong\.com' } ; then # trying to make this work with lesswrong.com  and alignmentforum.org
        urls="$(ec $urls | lw2gw)" @TRET
        urls="$(ec $urls | urlfinalg1)" @TRET # urlfinalg2 (unalix) doesn't handle the redirects here, idk why
        urls="$(ec $urls | rg 'greaterwrong\.com')" @TRET
        assert not isSpace urls @RET
    fi

    ec $urls | rg '/posts/' | url-clean-hash | guniq
}
##
