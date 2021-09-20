##
function w2e-lw-raw() {
    w2e-curl "$1" "${(@f)$(lw2gw "${@:2}")}"
    # transformer lw2gw "w2e-curl $(gq "$1")" "${@:2}"
    # transformer urlfinalg "transformer lw2gw w2e-curl $(gq "$1")" "${@:2}"
}

function tllw() {
    # we can't use an alias because tl won't get the correct URLs then.
    tll "${(@f)$(lw2gw "${@}")}"
}
noglobfn tllw

function lw2gw() {
    local from='(lesswrong\.com|alignmentforum\.org)' to='greaterwrong.com'

    rgx "$1" "$from" "$to"
}
reify lw2gw
enh-urlfinal lw2gw
noglobfn lw2gw
##
function lwseq() {
    mdoc "Usage: [tl options] URL ...
    Creates an ebook out of the sequences specified." MAGIC
    local opts
    zparseopts -A opts -K -E -D -M -verbose+=v v+ -prefix-title:=p p: -engine:=e e: -outputdir:=o o:
    re lw2gw "$@" | inargsf getlinks-c | command rg -F lesswrong.com/ | inargsf re lw2gw |inargsf tl -e "${opts[-e]:-w2e-curl}" -p "${opts[-p]}" -o "${opts[-o]:-./}"
}
noglobfn lwseq
##
function p-lw-urls {
    # tested with greaterwrong
    ##
    p-getlinks | rg '/posts/' | url-clean-hash | guniq
}
##
