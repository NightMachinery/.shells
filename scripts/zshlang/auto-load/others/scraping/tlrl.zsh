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
function tlrlu(){
    tlrl-ng "$@" -p "$(url-tailf "$1") | "
}
##
function tlrl-code(){
    tlrl-ng -e w2e-code -p "[$(url-tailf "$1")] " "$@"
}
##
function tlrl-gh() {
    tlrl-ng -e w2e-gh -p "[$(url-tailf "$1")] " "$@"
}

function tlrl-ng {
    mdoc "Usage: $0 [OPTIONS] <url> ...
Description: Automatically infers the title and the author from the first URL, and feeds all URLs into 'w2e'.
Options:
-p, --prefix-title <string>    Prepends the specified string to the title of the page. (Optional)
-e, --engine <function> Which zsh function to use for generating the book. Default is w2e-raw. (Optional)
-o, --outputdir <dir> Output directory, defaults to a tmp location. (Optional)
-v, --verbose ignored. Supported only for backwards-compatibility." MAGIC
    local opts e
    zparseopts -A opts -K -E -D -M -verbose+=v v+ -prefix-title:=p p: -engine:=e e: -outputdir:=o o:
    # dact typeset -p opts argv

    local title author
    if false ; then
        # Old API
        silent wread "$1" html || { ecerr "tlrl-ng: wread failed with $? on url $1" ; return 33 }
        title="${wr_title:-$1}"
        author="$wr_author"
    else
        url2note "$1" none || { ecerr "tlrl-ng: url2note failed with $? on url $1" ; return 33 }
        title="${tlrl_title:-${title:-untitled $1}}"

        : 'Note that readest is obviously only for the FIRST link.'
        author="[$readest] ${tlrl_author:-${author}} $(url-date "$1")"
    fi
    title="$( ec "${opts[-p]}${title}" | sd / _ )"
    title="${title[1,80]}"

    pushf "${opts[-o]:-$HOME/tmp-kindle}"
    we_author=$author eval "$(gq "${opts[-e]:-w2e-raw}" "$title" "$@")"
    e=$?
    popf
    return $e
}
@opts-setprefix tlrl-ng tlrl
noglobfn tlrl-ng
##
function outlinify() {
    mapln 'https://outline.com/$1' "$@"
}
##
function wread-bat() {
    unbuffer bat --theme OneHalfLight --pager=never --style=plain "$1" | aha --title "$(basename "$1")"
}

function tlbat() {
    uf_idem=y we_dler="wread-bat" w2e "$(basename "$1")" "$@"
}
##
function w2e-selectors {
    # You can use =fnswap 'w2e' 'pcz w2e'= to copy the invocation.
    ##
    local url="${w2e_url:-${$(browser-current-url)%%/}}"
    local title="${1:-$(browser-current-title)}" sel="${w2e_sel:-$2}"
    assert-args url title sel @RET

    full-html2 "$url" \
        | htmlq "$sel" \
        | urls-extract \
        | urls-cleansharps \
        | inargsf w2e "$title"
}
@opts-setprefix w2e-selectors w2e

function w2e-juliadocs {
    @opts sel '.docs-menu' @ \
        w2e-selectors "$@"
}
@opts-setprefix w2e-juliadocs w2e
##
