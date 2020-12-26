# imports json.zsh
### Aliases
alias withtxt="we_dler=readmoz-txt h2ed=txt2epub " # better for tables
alias tlcode='tlrl-code'
alias tlgh='tlrl-gh'
alias gurl='curlm -o /dev/stdout'
aliasfn withchrome 'fhMode=curlfullshort' reval
alias w2e-noredirect='fnswap urlfinalg arrN w2e' # readmoz uses full-html2 under the hood.
alias tlnoredirect='tlrl-ng -e w2e-noredirect'
aliasfn tlnor tlnoredirect
alias w2e-chrome='withchrome w2e-noredirect' # readmoz uses full-html2 under the hood.
alias tlchrome='tlrl-ng -e w2e-chrome'
aliasfn tlf tlchrome
alias w2e-curl-wayback='we_dler=wread-curl w2e-wayback'
##
alias wread-c='fhMode=curl wr_force=y wread'
###
function tlarc() {
    fhMode=curlfullshort transformer to-archive-is tl "$@"
}
function tlold() {
    arcMode=oldest transformer to-archive-is tl "$@"
}
function to-archive-is() {
    doc "Warning: archive.is is quick to ban bot-like behavior."

    # local url="$(urlfinalg "$1")"
    local url="$1" # dead URLs have a tendency to redirect to hell
    local mode="${arcMode:-full}"

    if ! match-url2 "$url" ; then
        ec "$url"
        return 0
    fi
    if [[ "$mode" == full ]] ; then
        # needs JS
        ec "https://archive.vn/?run=1&url=$url"
    elif [[ "$mode" == oldest || "$mode" == newest ]] ; then
        # will fail if no snapshots exist
        # might need fhMode=aa
        ec "http://archive.is/$mode/$url"
    else
        ecerr "$0: Invalid mode '$mode'"
        return 1
    fi
}
function tlwb() {
    local opts urls
    opts-urls "$@"
    # we can't use an alias because tl won't get the correct URLs then.
    tl $opts[@] "${(@f)$(wayback-url "${urls[@]}")}"
}
noglobfn tlwb

function wgetm() {
    wget --header "$(cookies-auto "$@")" "$@"
}
function curlm() {
    # cookie-jar saves cookies. I have it here to make curl activate its cookie engine.
    curl --silent --fail --location --cookie-jar /dev/null --header "$(cookies-auto "$@")" "$@"
}
function curl-useragent() {
    # Copy from Chrome's network pane
    curl --fail --no-progress-meter \
        -H 'upgrade-insecure-requests: 1' \
        -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36' \
        -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' \
        -H 'accept-language: en-US,en;q=0.9,fa;q=0.8,ru;q=0.7,ur;q=0.6' \
        --compressed "$@"
}
function web-lastmod() {
    curlm -I "$1" 2>&1 | rg --smart-case last-modified
}
function cookies-copy() {
    ec-copy "theCookies=$(gq "$(cookies-auto "$@")" "$@")"
}
aliasfn cook cookies-copy
function cookies-killlock() {
    kill "$(serr fuser "$cookiesFile")"
}
getcookies() {
    mdoc "[cookiesFile= ] $0 <url>
Will output Chrome's cookies for the given URL in key=val;key2=val2
See |cookies| for higher-level API." MAGIC
    local url="$1"
    local cf="${cookiesFiles}"

    test -e "$cf" || { ecdbg "getcookies called with non-existent file: $cf" ; return 0 }
    cookiesFile="$cf" url="$url" python -c '
from pycookiecheat import chrome_cookies
import os
url = os.environ["url"]
HOME = os.environ["HOME"]
cookiesFile = os.environ["cookiesFile"]
cookies = chrome_cookies(url, cookie_file=cookiesFile)
#print(cookies)
out = ""
for k,v in cookies.items():
    out += f"{k}={v};"
print(out[:-1])
'

}
cookies() {
    mdoc "$0 [<cookie-or-url>=theCookies]
Outputs in header style." MAGIC

    local url="$1"
    local env_c="$theCookies"
    local c="Cookie:$1"

    if [[ "$url" =~ '^http' ]]
    then
        c="Cookie:$(getcookies "$url")"
    fi
    test -z "$url" && c="Cookie:${env_c}"
    ec "$c"
}
function cookies-auto() {
    mdoc "Returns theCookies if present. Otherwise tries to get the cookies from the first url in args." MAGIC

    test -n "$caDisableCookies" && return 0

    local ci=''
    if test -z "$theCookies"
    then
        local i
        for i in $@
        do
            if [[ "$i" =~ '^http' ]]
            then
                c="$(cookies "$i")"
                break 
            fi
        done
    else
        c="$(cookies)"
    fi

    ec "$c"
}
function wread() {
    mdoc "[wr_force=] $0 [--file <file>] <url> [<output-format>]
Global output: wr_title wr_author" MAGIC
    local file=''
    [[ "$1" == '--file' ]] && {
        test -e "$2" && file="$(realpath "$2")" || { ecerr "wread called with nonexistent file." ; return 33 }
        shift 2
    }
    setopt local_options pipefail
    local title author
    test "${2:=markdown}" = 'html' && title='"<h1>"+.title+"</h1>"' || title='"# "+.title'
    test "${2}" = 'html' && author='"<p>By: <b>"+.author+"</b></p>"' || author='"By: **"+.author+"**"'
    local merc="$({ test -z "$file" && { # No file, downloading
        test -z "$wr_force" && { mercury-parser --format="${2}" "$(urlfinalg "$1")" || return $? } || {
                  fu_wait="${fu_wait:-60}" aget "full-html $1:q ./a.html ; mercury-html $1:q ./a.html $2:q"
              } } || {
          # File supplied
          aget "cat ${(q@)file} > a.html ; mercury-html $1:q ./a.html $2:q"
      } })"
    # " LINTERBUG
    wr_title="$(<<<"$merc" jqm -r .title | tr '\n' ' ')"
    wr_author="$(<<<"$merc" jqm -r .author)"
    <<<"$merc" jq -e --raw-output 'if .content then [
    (if .title then '"$title"' else empty end),
    (if .author then '"$author"' else empty end),
    .content
] | join("\n\n") else null end'
}
function mercury-html() {
    doc USAGE: url html-file output-mode
    serr mercury-html.js "$@"
}

function httpm() {
    local c="$(cookies-auto "$@")"
    http --style solarized-light --follow --ignore-stdin --session "pink$(uuidpy)" "$@" $c
}
function full-html2() {
    # wget, aa, curl fail for https://www.fanfiction.net/s/11191235/133/Harry-Potter-and-the-Prince-of-Slytherin
    # seems to be because the server is messed up, but whatever:
    # http: error: Incomplete download: size=46696; downloaded=131310


    # local mode="${fhMode:-http}"
    local mode="${fhMode:-cloudscraper}"
    local url="$1"

    if [[ "$url" =~ '^(?:https?://)?[^/]*techcrunch\.' ]] ; then
        ecdbg "$0: Techcrunch Mode"
        techcrunch-curl "$url"
        return $?
    elif [[ "$url" =~ '^(?:https?://)?[^/]*t\.co/' ]] ; then # techmeme sometimes links to FT using these
        ecdbg "$0: t.co Mode"
        mode='curlfull'
    fi

    [[ "$mode" =~ '^curlfast$' ]] &&  { $proxyenv curl --silent --fail --location -o /dev/stdout "$url" ; return $? }
    [[ "$mode" =~ '^curlfullshort$' ]] &&  { cfTimeout=1 $proxyenv curlfull.js "$url" ; return $? }
    [[ "$mode" =~ '^curlfull$' ]] && { cfTimeout=20 $proxyenv curlfull.js "$url" ; return $? }
    [[ "$mode" =~ '^curlfulllong$' ]] && { cfTimeout=900 $proxyenv curlfull.js "$url" ; return $? }
    [[ "$mode" =~ '^aa(cookies)?$' ]] && {
        local tmp="$(uuidgen)"
        local tmpdir="$(get-tmpdir)"
        $proxyenv dbgserr aacookies "$url" --dir "$tmpdir" -o "$tmp" || return $?
        < "$tmpdir/$tmp"
        return $?
        # Note that -o accepts basenames not paths which makes it incompatible with any /dev/* or other special shenanigans
    }
    [[ "$mode" =~ '^(c|g)url$' ]] && { $proxyenv gurl "$url" ; return $? }
    [[ "$mode" =~ '^cloudscraper$' ]] && { 
	    $proxyenv cloudscraper_get.py "$url" # idk if proxyenv works for this
            return $?
    }
    [[ "$mode" =~ '^http(ie)?$' ]] && {
        $proxyenv dbgserr httpm "$url"
        return $?
    }
}
function full-html() {
    fhMode="${fhMode:-curlfull}" full-html2 "$1" > "$2"
    return "$?"

    # local mode="${fhMode:-curlfull}"
    # [[ "$mode" =~ 'curlfull' ]] && curlfull.js "$1" > "$2"
    # [[ "$mode" =~ 'aa(cookies)?' ]] && dbgserr aacookies "$1" -o "$2" # Note that -o accepts basenames not paths
    # [[ "$mode" =~ '(c|g)url' ]] && gurl "$1" > "$2"
    # [[ "$mode" =~ 'http(ie)?' ]] && http --session pink "$1" --output "$2"

    #doc splash should be up. https://splash.readthedocs.io
    #doc 'wait always waits the full time. Should be strictly < timeout.'
    #curl --silent "http://localhost:8050/render.html?url=$1&timeout=90&wait=${fu_wait:-10}" -o "$2"
}
function random-poemist() {
    curl -s https://www.poemist.com/api/v1/randompoems |jq --raw-output '.[0].content'
}
xkcd() {
    wget `wget -qO- dynamic.xkcd.com/comic/random | sed -n 's/Image URL.*: *\(\(https\?:\/\/\)\?\([\da-z\.-]\+\)\.\([a-z\.]\{2,6\}\)\([\/\w_\.-]*\)*\/\?\)/\1/p'`
}
##
wayback() {
    comment -e, --exact-url
    comment "-t, --to TIMESTAMP Should take the format YYYYMMDDhhss, though
                        you can omit as many of the trailing digits as you
                        like. E.g., '201501' is valid."
    comment '-p, --maximum-snapshot NUMBER    Maximum snapshot pages to consider (Default is 100)'
    comment '-d, --directory PATH             Directory to save the downloaded files into
    Default is ./websites/ plus the domain name'

    wayback_machine_downloader -e -t "${wa_t:-20170505152803}" -d ./ -p 1 "$@"
}
wayback-out() {
    aget "wayback $(gquote "$@") ; cat *.html"
}
wread-wayback() {
    wayback-out "$1" | wread --file /dev/stdin "$@"
}
function wayback-url() {
    # --to-date "${wa_t:-2017}" --from-date 2000
    # outputs from oldest to newest
    waybackpack --list "$1" | sponge | head -n1
}
# enh-urlfinal wayback-url ## old URLs often redirect to hell
reify wayback-url
noglobfn wayback-url
##
function w2e-curl() {
    we_dler=wread-curl w2e "$@"
}
noglobfn w2e-curl
function wread-curl() {
    full-html2 "$1"
    # gurl "$1"
}
function w2e-gh() {
    h2ed=html2epub-pandoc-simple w2e-curl "$1" "${(@f)$(gh-to-readme "${@:2}")}"
}
noglobfn w2e-gh

function url-exists() {
    local ret=1 url="$1"

     # Don't use --head, it doesn't work with some urls, e.g., https://github.com/Radarr/Radarr/wiki/Setup-Guide.md . Use `-r 0.0` to request only the first byte of the file.
    curl --output /dev/null --silent -r 0-0 --fail --location "$url" && {
        ret=0
        url_exists_out="${url_exists_out:-$url}" # only set it if it's not set before. This helps us try  a bunch of URLs and find the first one that exists.
    }
    return "$ret"
}
function gh-to-readme() {
    local urls=() i i2 readme url exts=(md rst org) readmes=(readme README ReadMe readMe Readme)

    for i in "$@"
    do
        ! [[ "$i" =~ 'github.com' ]] || [[ "$i" == *.(${(j.|.)~exts}) ]] ||
            {    i2="${i}.md"
                 comment we hope to handle wiki pages with this method, but beware that nonexistent wiki pages trigger create a new page, not the desired not existent response.
                 unset url_exists_out
                 re url-exists "$i2" "${i}/blob/master/${^readmes[@]}.${^exts[@]}"
                 i="$url_exists_out"
            }
        url-exists "$i" && urls+="$i" || color red "$i does not seem to exist." >&2
    done
    gh-to-raw "$urls[@]"
}
function gh-to-raw() rex 'rgx _ /blob/ /raw/' "$@"
function url-final-hp() {
    # Sometimes returns wrong result:
    # https://www.arvarik.com/the-stable-marriage-problem-and-modern-datingi:
    # HTTP/1.1 302 Moved Temporarily
    # Location: /the-stable-marriage-problem-and-modern-dating/

    local out="$(2>&1 httpm --all --follow --headers "$1" )"
    # ec $out
    local res

    res="$(<<<"$out" command rg --only-matching --replace '$1' 'Location: (.*)'  | tail -n1 )"
    test -n "$res" && ec $res || ec "$1"
}
renog url-final-hp
function url-final() {
    # beware curl's retries and .curlrc
    curlm -o /dev/null -w %{url_effective} "$@" #|| ec "$@" # curl prints urls even if it fails ...
    ec # to output newline
}
function url-final2() {
    doc "This one doesn't download stuff."
    doc 'WARNING: Can eat info. E.g., https://0bin.net/paste/5txWS7vyTdaEvNAg#QJZjwyNoWYyaV5-rqdCAcV7opxc+kyaMwoQ7wyjLjKy'
    [[ "$(2>&1 wgetm --no-verbose --spider "$1" )" =~ '.* URL: (.*) 200 .*' ]] && ec "$match[1]" || url-final "$1"
}
function url-final3() {
    doc 'The most reliable and expensive way.'
    # too expensive
    # retry-limited 3 urlfinal.js "$1" || url-final2 "$1"
    url-final2 "$1"
}
reify url-final url-final2 url-final3
noglobfn url-final url-final2 url-final3
function url-tail() {
    [[ "$1" =~ '\/([^\/]+)\/?$' ]] && ec "$match[1]" || ec "$1"
}
reify url-tail
noglobfn url-tail
function tlrlu(){
    tlrl-ng "$@" -p "$(url-tailf "$1") | "
}
function tlrl-code(){
    tlrl-ng -e w2e-code -p "[$(url-tailf "$1")] " "$@"
}
function url-tailf() {
    ec "$(url-tail "$(urlfinalg "$1")")"
}
function url-tailedtitle() {
    ec "$(url-title "$1") $(url-tail "$(urlfinalg "$1")")"
}
renog url-tailedtitle
function url-title() {
    urlmeta2 "${1:?}" title
}
renog url-title
function tlrl-gh() {
    tlrl-ng -e w2e-gh -p "[$(url-tailf "$1")] " "$@"
}
function tlrl-ng() {
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
        title="${title:-untitled $1}"
        : 'Note that readest is obviously only for the FIRST link.'
        author="[$readest] $author $(url-date "$1")"
    fi
    title="$( ec "${opts[-p]}${title}" | sd / _ )"
    
    pushf "${opts[-o]:-$HOME/tmp-kindle}"
    we_author=$author eval "$(gq "${opts[-e]:-w2e-raw}" "$title" "$@")"
    e=$?
    popf
    return $e
}
noglobfn tlrl-ng
outlinify() {
    mapln 'https://outline.com/$1' "$@"
}
html2epub-calibre() {
    mdoc "Usage: $0 <title> <authors> <html-file> ..." MAGIC
    local authors="${2:-nHight}"
    local title="$(<<<$1 gtr '/' '.')"

    local u="$title $(uuidgen).html"

    merge-html "${@:3}" > "$u"
    ebook-convert "$u" "$title.epub" \
        --authors="$authors" \
        --level1-toc="//*[name()='h1' or name()='h2']" \
        --level2-toc="//h:h3" \
        --level3-toc="//*[@class='subsection']" \
        --page-breaks-before="//*[(name()='h1' or name()='h2') or @class='owner-name']" \
        --use-auto-toc --toc-threshold=0 \
        --toc-title="The TOC" \
        --embed-all-fonts \
        --title="$title" --epub-inline-toc --enable-heuristics
    \rm "$u"
}
txt2epub () {
    "${t2ed:-txt2epub-pandoc}" "$@"
}
txt2epub-calibre() {
    mdoc "DEPRECATED. Outputs weird like â€™ for apostrophe. Also only accepts single input.
Use txt2epub-pandoc.
Usage: $0 <title> <authors> <txt-file>" MAGIC
    local u="$3" title="${1}" authors="${2:-nTight}"
    ebook-convert "$u" "$title.epub" \
        --authors="$authors" \
        --title="$title" --epub-inline-toc --enable-heuristics

}
t2e() {
    txt2epub "$1" "${te_author:-night_t2e}" "${@:2}"
    p2k "$1".epub
}
function html2epub() {
    ecdbg calling "${h2ed:-html2epub-calibre}" "$@"
    local files=( "$@[3,-1]" )

    # filter0 ishtml-file # don't do this, as we download non-html files in w2e-curl
    arr0 "$files[@]" | inargs0 "${h2ed:-html2epub-calibre}" "$1" "$2"
}
html2epub-pandoc() {
    # title author htmls
    local title="$1"
    local author="$2"

    PANDOC_FORMAT=html-native_divs 2epub-pandoc-byformat "$title" "$author" <(merge-html "${@:3}")
    # pandoc --toc -s -f html-native_divs <(merge-html "${@:3}") --metadata title="$title" --epub-metadata <(ec "<dc:title>$title</dc:title> <dc:creator> $author </dc:creator>") -o "$title.epub"
}
h2e() {
    html2epub "$1" "${h2_author:-night}" "${@:2}" && ec "Book '$1' created." || ecerr "$0 failed for book: $1"
    p2k "$1".epub
}
function web2epub() {
    doc usage: 'we_strict= we_retry= we_dler= we_author= title urls-in-order'
    local title="$(<<<$1 gtr '/' '.')"
    local u="$title $(uuidgen)"
    cdm "$u"
    local author="$(<<<${we_author:-night} gtr '/' '.')"
    local i=0
    local hasFailed=''
    local strict="$we_strict"

    local dled_files=()
    # FNSWAP: urlfinalg
    for url in "${(@f)$(urlfinalg "${@:2}")}"
    do
        local bname="$(url-tail "$url")"  #"${url##*/}"
        #test -z "$bname" && bname="u$i"
        bname="${(l(${##})(0))i} ${bname}" # Do NOT add an extension here, so that w2e-curl works.
        i=$((i+1))

        # API Change; old: "${we_dler:-wread}"
        if retry-limited-eval "${we_retry:-10}" "${we_dler:-readmoz}" "$url:q" '>' "$bname:q" && ec "Downloaded $url ..." ; then
            dled_files+="$bname"
        else
            command rm "$bname" # delete partial or empty files
            ec "$url" >> failed_urls
            ecerr "Failed $url"
            hasFailed=y
        fi
    done

    if test -n "$hasFailed" ; then
        ecerr "Some urls failed (stored in $(pwd)/failed_urls)."
        if test -n "$strict" ; then
            ecerr 'Strict mode is enabled; Create the epub manually.'
            return 1
        fi
    fi
    if (( $#dled_files == 0 )) ; then
        ecerr "$0: No files were downloaded successfully."
        # cd '../' # idk if this'd be good
    else
        ec "Converting to epub ..."
        revaldbg html2epub "$title" "$author" "$dled_files[@]"
        mv *.epub ../ && cd '../' &&  { { isDbg || test -n "$hasFailed" } || \rm -r "./$u" }
        ec $'\n\n'"Book '$title' by '$author' has been converted (hasFailed='${hasFailed}')."$'\n\n'
    fi
}
function w2e-raw() {
    local title="$(<<<$1 gtr '/' '.')"
    web2epub "$title" "${@:2}" && p2k "$title.epub"
}
function w2e-o() {
    w2e-chrome "$1" "${(@f)$(outlinify "${@:2}")}"
}
noglobfn w2e-o
function w2e-wayback() {
    w2e-raw "$1" "${(@f)$(wayback-url "${@:2}")}"
}
noglobfn w2e-wayback
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
    rgx "$1" 'lesswrong\.com' greaterwrong.com
}
reify lw2gw
enh-urlfinal lw2gw
noglobfn lw2gw

function code2epub() {
    mdoc "Usage: we_author=<author> $0 <title> <sourcecode> ..." MAGIC

    local title="$1"
    local author="$we_author"

    PANDOC_FORMAT=markdown 2epub-pandoc-byformat "$title" "$author" <(code2md "${@[2,-1]}")
}
function 2epub-pandoc-byformat() {
    : "PANDOC_FORMAT= <name> <author> <file with that format> ..."

    local format="$PANDOC_FORMAT"
    local title="$1"
    local author="$2"
    local files=( "${@:3}" )

    test -z "$format" && { 
        ecerr "$0: Empty format supplied"
        return 1
    }

    2epub-pandoc-simple "$title" "$author" -f "$format" "$files[@]"
    # pandoc --toc -s --metadata title="$title" --epub-metadata <(ec "<dc:title>$title</dc:title> <dc:creator> $author </dc:creator>") -o "$title.epub" -f "$format" "$files[@]"
}
function 2epub-pandoc-simple() {
    : "This works for any files that have the correct extension, or if you set the format explicitly."
    : "<name> <author> (<file with ext> OR <pandoc-opt>) ..."

    local title="$(<<<$1 gtr '/' '.')"
    local author="$(<<<$2 gtr '/' '.')"

    pandoc --toc -s "${@:3}" --metadata title="$title" --epub-metadata <(ec "<dc:title>$title</dc:title> <dc:creator> $author </dc:creator>") -o "$title.epub"
}
aliasfn html2epub-pandoc-simple 2epub-pandoc-simple
function 2epub-pandoc-byext () {
    magic mdocu "[PANDOC_EXT=txt] <title> <author> <file> ..." ; mret
    local ext="${PANDOC_EXT:-txt}"
    local title="$1"
    local author="$2"
    local files=( "${@:3}" )
    local txts=()
    local i
    for i in "$files[@]" ; do
        local t="$(gmktemp --suffix ."$ext")"
        cp "$i" "$t"
        txts+="$t"
    done
    2epub-pandoc-simple "$title" "$author" "$txts[@]"
}
aliasfn txt2epub-pandoc PANDOC_EXT=txt 2epub-pandoc-byext
aliasfn md2epub-pandoc PANDOC_EXT=md 2epub-pandoc-byext
function aa2e() {
    ecerr DEPRECATED: Use w2e-curl.
    aget "aa -Z $(gquote "${@:2}")
html2epub-pandoc-simple $1:q ${${author:-aa2e}:q} *
mv $1:q.epub ../"
    p2k "$1".epub
}
function code2html() {
    mdocu '[chroma-options] file -> HTML in stdout' MAGIC
    chroma --formatter html --html-lines --style trac "$@"
}
function code2html-url() {
    code2html "${@:2}" <(full-html2 "$1")
}
function wread-code() {
    code2html-url "$1"
}
function w2e-code-old() {
    doc DEPRECATED: The html produced is not bad but after conversion we lose newlines which just sucks.
    we_dler=wread-code w2e "$1" "${(@f)$(gh-to-raw "${@:2}")}"
}
function w2e-code() {
    mdocu '<name> <url> ...' MAGIC
    aget aa -Z "$(gquote "${(@f)$(gh-to-raw "${@:2}")}")" \; code2epub "$1:q" '*' \; mv ${1:q}.epub ../
    p2k ${1}.epub
}
function code2md() {
    local i
    for i in "$@"
    do
        ec "

# ${i:t}

"'```'"${i:e}
 $(cat $i)
"'```'
    done
}
function html-get-reading-estimate() {
    local est
    est="$(cat "$1" | readtime.js)"
    ec "$(ec $est|jqm .humanizedDuration) ($(ec $est|jqm .totalWords) words)"
}
function merge-html() {
    (( $# == 1 )) && {
        ec "<p>$(html-get-reading-estimate $1)</p>

"
        cat $1
        return $?
    }
    local i
    for i in "$@"
    do
        ec "<h1>$(strip $i ".html") - $(html-get-reading-estimate $i)</h1>

$(cat "$i")
"
    done
}
function getlinks() {
    lynx -cfg=~/.lynx.cfg -cache=0 -dump -nonumbers -listonly $1|grep -E -i ${2:-'.*'}
}
function getlinksoup() {
    getlinks.py "$1" | command rg "${@[2,-1]:-.*}"
}
noglobfn getlinks getlinksoup
function getlinksfull() {
    mdoc "$0 <link> [ options for rg ]
Remember that you can customize full-html by fhMode." MAGIC

    local url="$1"
    local u
    u="$(uuidpy)"
    full-html "$url" "$u"
    test -e "$u" || { ecerr "${0}: Couldn't download $url" ; return 1 }
    getlinks.py "$1" "$u" | trimsed | command rg "${@[2,-1]:-.*}"
    \rm "$u"
}
noglobfn getlinksfull
function lwseq() {
    mdoc "Usage: [tl options] URL ...
    Creates an ebook out of the sequences specified." MAGIC
    local opts
    zparseopts -A opts -K -E -D -M -verbose+=v v+ -prefix-title:=p p: -engine:=e e: -outputdir:=o o:
    re lw2gw "$@" | inargsf getlinks-c | command rg -F lesswrong.com/ | inargsf re lw2gw |inargsf tl -e "${opts[-e]:-w2e-curl}" -p "${opts[-p]}" -o "${opts[-o]:-./}"
}
noglobfn lwseq
function urlfinalg() {
    doc supports Google redirects. Set uf_idem to y to return original.
    local URL="$1"
    { test -n "$uf_idem" || ! match-url "$URL" } && {
        ec "$URL"
        return 0
    }
    local u="$URL"
    [[ "$URL" =~ "^(http(s)?://(www\.)?)?google\.com/.*" ]] && {
        #u=`echo "$URL" | perl -n -e '/url=([a-zA-Z0-9%\.]*)/ && print "$1\n"'`
        [[ "$URL" =~ "url=([^&]*)" ]] && u="$match[1]" || { ecerr failed to decode Google url ; return 1 }
        u="$(ec $u | url-decode.py)"
    }
    url-final "$u" #url-final2 sometimes edits URLs in bad ways, while url-final downloads them.
}
reify urlfinalg
noglobfn urlfinalg
function jwiki() {
    serr jwiki.py "$*" 1
}
function wread-man() {
    local m=""
    m="$(MAN_KEEP_FORMATTING=1 COLUMNS=70 serr man "$1")" && m="$(<<<"$m" command ul)" || m="$(2>&1 "$1" --help)" || { ecerr "$0 failed for $1" ; return 1 }
    <<<"$m" aha --title "$1"
}
function tlman() {
    uf_idem=y we_dler="wread-man" w2e "$1" "$@"
}
function wread-bat() {
    unbuffer bat --theme OneHalfLight --pager=never --style=plain "$1" | aha --title "$(basename "$1")"
}
function tlbat() {
    uf_idem=y we_dler="wread-bat" w2e "$(basename "$1")" "$@"
}
function getlinksfull2() {
    mdoc "$0 [-e,--regex <flitering-regex>] <url> ...
Uses getlinksfull (full-html) under the hood." MAGIC
    local opts
    zparseopts -A opts -K -E -D -M -regex:=e e:
    local pages=("$@")
    local regex="${opts[-e]:-.*}"
    # re dvar pages regex
    for page in "$pages[@]"
    do
        getlinksfull "$page" "$regex"
    done
    # zargs is buggy with its quotation
    # zargs --verbose -i _ -- "$pages[@]" -- getlinksfull _ "$regex"
}
function getlinks2() {
    fhMode="${fhMode:-curl}" getlinksfull2 "$@"
}
function getlinks-c() {
    fhMode="${fhMode:-aacookies}" getlinksfull2 "$@"
}
function getlinks-uniq() {
    getlinks-c "$@" | gsort --unique
}
function aamedia() {
    mdoc "$0 <page-url> ...
Scrapes media and audio links from the given pages, and then downloads them. Uses cookies (might need more config)." MAGIC

    local urls=( ${(u@)@} )

    local formats=( ${media_formats[@]} pdf )
    local regex='\.('"${(j.|.)formats}"')$'
    local url
    for url in ${urls[@]}
    do
        url="$(urlfinalg $url)"
        [[ "$url" =~ "$regex" ]] && ec $url || getlinks-c -e $regex "$url"
    done | gsort -u | {
        if isDbg
        then
            color 150 0 255 "$(cat)"
        else
            inargsf aacookies -Z
        fi
    }
}
function aaCW() {
    mdoc "$0 <url> ..." MAGIC
    local theCookies=${theCookies:-"$(cookies $1)"}
    getlinks-c -e 'resource/view\.php' "$@" | inargsf aamedia
}
function ygen() {
    y --force-generic-extractor "$@"
    rename .apt .mp4 *.apt
}
noglobfn ygen aaCW aamedia
function hi10-jtoken() {
    hi10jtoken.js

    # doc "Doesn't work. Probably because of their custom MD5 function."
    # local jtoken="$(head -n1 /dev/random |md5)"
    # jtoken="${jtoken[1,5]}"
    # local id="$(<<<"$jtoken" md5)"
    # id="${id[1,5]}"
    # re dvar jtoken id
    # ec "?jtoken=${jtoken}${id}"
}
function urlmeta2() {
    mdoc "[html= ] $0 <url> <req> ...
gets the requested metadata. If html is supplied, will use that. In that case, <url> is superfluous." MAGIC

    local url="$1"
    local html="${html:-$(full-html2 "$url")}"
    local reqs=("${@:2}")
    <<<"$html" htmlmetadata $reqs[@]
}
function urlmeta() {
    mdoc "DEPRECATED: Use urlmeta2.
[html= ] $0 <url> <req>
gets the requested metadata. If html is supplied, will use that. In that case, <url> is superfluous." MAGIC

    local url="$1"
    local html="${html:-$(full-html2 "$url")}" f="$(mktemp)"
    ec $html > $f # big env vars cause arg list too long
    htmlf=$f req="${2:-title}" python3 -W ignore -c "
import metadata_parser, os, sys
page = metadata_parser.MetadataParser(html=open(os.environ['htmlf'], 'r').read())
if os.environ.get('DEBUGME',''):
   print(page.metadata, file=sys.stderr)
   # from IPython import embed; embed()
req=os.environ['req']
if req == 'all' :
    
    print(page.get_metadata('title') or '', end='\x00')
    print(page.get_metadata('description') or '', end='\x00')
    print(page.get_metadata('image') or '', end='\x00')
    print(page.get_metadata('author') or page.get_metadata('creator') or page.get_metadata('article:author') or '', end='\x00')
else:
    print(page.get_metadata(req) or '')
"
    \rm -f $f
}
function hi10-cook() {
    local url="$1"
    local title="$(urlmeta $url title|str2tmuxname)"
    local cmd="FORCE_INTERACTIVE=y $(cook hi10-rc hi10-ng "$url")"
    ec-copy "tmux new -s $(gq "$title") zsh -c $(gq "$cmd")"
}
function hi10-ng() {
    mdoc "$0 <url-of-hi10-page> [<regex>]
Use hi10-cook to copy the necessary command for pasting in a remote server." MAGIC
    local url="$1"
    local regex=${2:-'\.mkv$'}

    pxa-maybe
    local title="$(urlmeta $url title)"
    [[ "${$(pwd):t}" == "$title" ]] || cdm "$title"
    getlinks-c "$url" -e "$regex" | inargsf hi10-multilink
}
function hi10-multilink() {
    local argCount=$#
    local pArgs=()
    local i
    for (( i=1; i<=$argCount; i+=1 ))
    do
        if [[ "$argv[i]" =~ '(https?://[^/]*hi10anime.*)' ]]; then #'.*http:\/\/ouo.io\/s\/166CefdX\?s=(.*)' ]]; then
            # echo $match[1]
            pArgs[$i]="${match[1]}"
            # pArgs[$i]='http://hi10anime'"${match[1]}$(hi10-jtoken)"
        else
            ecerr Invalid link: "$argv[i]"$'\n'
        fi
    done
    # echo $pArgs
    # --referer="$1" is not needed now, if needed be sure to use regex matching to give it, as the urls returned from lynx are invalid.
    if isDbg
    then
        arger "${(@u)pArgs}"
    fi
    doc use fz for filtering and ordering
    arrN "${(@u)pArgs}" | fz | tee "hi10-links.txt" | hi10-dl # (u) makes the array elements unique.
}
function hi10-dl() {
    magic mdoc "$0 < links.txt
Generates jtokens for links and downloads them." ; mret

    inargsf mapg '$i$(hi10-jtoken)' | inargsf rgeval aa -j2 -Z
}
function hi10-from-page() {
    mdoc "DEPRECATED. Use hi10-ng" MAGIC

    # You need to have persistent cookies in lynx, and have logged in.
    hi10-multilink "${(@f)$(lynx -cfg=~/.lynx.cfg -cache=0 -dump -listonly $1|grep -E -i ${2:-'.*\.mkv$'})}"
    # eval 'hi10-multilink ${(@f)$(lynx -cfg=~/.lynx.cfg -cache=0 -dump -listonly "'"$1"'"|grep -E -i "'"${2:-.*\.mkv$}"'")}'
}
function libgendl-md5-main() {
    local md5="$1"
    local mainmirror="http://93.174.95.29"
    # local url="http://gen.lib.rus.ec/get?md5=${md5}&open=0"
    local urls=( "$mainmirror/main/${md5}" "$mainmirror/fiction/${md5}" )
    getlinks-c -e '\.[^/]+$' "$urls[@]" # will get false positives if the name of the book contains a dot. We can whitelist allowed formats but that is too costly ...
}
function libgendl-md5-bok() {
    local outs="$(libgendl-md5-bok-helper "$1" |inargsf bok.py)"
    test -e "$outs" # we expect only a single download
}
function libgendl-md5-bok-helper() {
    local md5="$1"
    local url="https://b-ok.cc/md5/$md5"
    getlinks-c -e '/book/' "$url" |gsort -u
}
function libgendl-md5-bok-old() {
    (( ${+commands[bok.js]} )) || { ecerr 'bok.js not found.' ; return 1 }
    libgendl-md5-bok-helper "$1" |inargsf re "gtimeout 15m bok.js"
}
function libgendl-md5-old() {
    local bok="$(libgendl-md5-bok-old $1)"
    if test -n "$bok" ; then
        aa "$bok"
    else
        libgendl-md5-main "$1" |inargsf aa -Z
    fi
}
function libgendl-md5() {
    local md5="$1"
    local lgNoBok="$lgNoBok"

    { test -z "$lgNoBok" && libgendl-md5-bok "$md5" } || {
        test -z "$lgNoBok" && ecerr "bok failed. Trying main ..."
        local links=( ${(@f)"$(libgendl-md5-main "$md5")"} )
        if (( ${#links} >= 1 )) ; then
          aa -Z $links[@]
        else
          ecerr "$0: No books found for md5: $md5"
          return 1
        fi
    }
}
reify libgendl-md5-main libgendl-md5-bok libgendl-md5-old libgendl-md5-bok-old libgendl-md5
function jlibplain() {
    # libgendl-md5-main "${(f@)$(re libgen2md5 "$@")}" | inargsf aa -Z
    libgendl-md5 "${(f@)$(libgen2md5 "$@")}"
    # serr re "libgen-cli download -o ." "${(f@)$(re libgen2md5 "$@")}"
}
noglobfn jlibplain
function jlib() {
    jee
    jlibplain "$@" || return 1
    dir2k
    true
}
function libgen2md5() {
    [[ "$1" =~ '(\w{32})\W*$' ]] && print -r -- "$match[1]"
}
reify libgen2md5
noglobfn libgen2md5
function jfic() {
    jee
    local i
    for i in "$@" ; do
        silent aa -- "$(urlmeta "$i" image)"
    done
    re "fanficfare --non-interactive" "$@"
    sout re p2k *.epub
}
##
function url2note() {
    magic mdoc "[ url2note_override_title= html= cleanedhtml= url2note_img ] $0 <url> [<mode>] ; outputs in global variables and stdout.
Set cleanedhtml=no to disable adding the reading estimate. (This improves performance.)" ; mret

    # test perf:
    # url='https://www.newyorker.com/culture/annals-of-inquiry/slate-star-codex-and-silicon-valleys-war-against-the-media' ; html="$(full-html2 "$url")"

    local url="$1"
    test -n "$url" || {
        return 1
    }
    # url="$(urlfinalg "$url")"
    local imgMode="${url2note_img}" emacsMode="${url2note_emacs}"

    if [[ "$url" =~ '^(?:https?://)?[^/]*youtube.com' ]] ; then
        imgMode=y
        if [[ "$url" =~ '^(?:https?://)?[^/]*youtube.com(?:/embed/([^/]*))' ]] ; then
            local id="$match[1]"
            img="https://i.ytimg.com/vi/${id}/maxresdefault.jpg" # embedded videos don't set their bloody meta tags
        fi
    fi
    local mode="${2:-md}"
    isLocal && local fhMode="${fhMode:-curlfast}" # servers are fast enough to work with the default fhMode

    local html="${html:-$(full-html2 "$url")}"
    local cleanedhtml="${cleanedhtml:-$(<<<"$html" readability "$url")}" # takes ~1.5s
    
    # old: # meta=( "${(@0)$(urlmeta $url all)}" ) # takes ~0.475s
    meta=( "${(@0)$(urlmeta2 $url title description image author)}" ) # takes ~0.04s
    title="${url2note_override_title:-$meta[1]}"
    desc="${meta[2]}"
    desc="$(<<<$desc html2utf.py)"
    img="${meta[3]:-$img}"
    author="$meta[4]"
    readest=""
    [[ "$cleanedhtml" != no ]] && readest="$(<<<"$cleanedhtml" html-get-reading-estimate /dev/stdin)" # takes ~0.25s

    local maxDesc=600
    if (( ${#desc} > $maxDesc )) ; then
        desc="${desc[1,$maxDesc]} ..."
    fi

    local indent="    "
    if [[ "$mode" == md ]] ; then
        ec "* [${title:-$url}]($url)"
        test -z "$author" || ec "${indent}* By: $author"
        test -z "$readest" || ec "${indent}* $readest"
        test -z "$desc" || ec "${indent}* $desc"
        #test -z "$title" || ec "${indent}* $url"
        test -n "$imgMode" && test -n "$img" && ec '![]'"($img)"
    elif [[ "$mode" == org ]] ; then
        if test -z "$emacsMode" ; then
            indent="  "
            ec "- [[$(ec $url| url-encode.py)][${title:-$url}]]"
        else
            # we insert the links with the heading already created in emacs.
            indent=""
            ec "[[$(ec $url| url-encode.py)][${title:-$url}]]"
        fi
        test -z "$author" || ec "${indent}+ By: $author"
        test -z "$readest" || ec "${indent}+ $readest"
        test -z "$desc" || ec "${indent}+ $desc"
        ##
        if test -n "$imgMode" && test -n "$img" ; then
            # ec "${indent}[[img$img]]"
            ##
            local ext="${${img:e}:-png}"
            local name="$(uuidm).$ext"
            local imgdir="$orgdir/images"
            mkdir -p "$imgdir"
            if silent $proxyenv aa "$img" --dir "$imgdir" -o "$name" ; then
                # local imgpath="$orgdir/images/$name"
                local imgpath="images/$name"
                ec "${indent}[[orgdir:$imgpath]]"
            else
                ecerr "$0: Failed to download: $img"
            fi
        fi
        ##
    elif [[ "$mode" == html ]] ; then
        test -z "$title" || ec "<h1>${title}</h1>"
        ec "<p>$url</p>"
        test -z "$author" || ec "<p>By: $author</p>"
        test -z "$readest" || ec "<p>${readest}</p>"
        test -z "$desc" || ec "<p>Description: $desc</p>"
        test -n "$imgMode" && test -n "$img" && ec "<img src=\"$img\" />"
    elif [[ "$mode" == none ]] ; then

    fi

}
noglobfn url2note
function url2org() { url2note "$1" org }
renog url2org
@opts-setprefix url2org url2note
function url2md() { url2note "$1" md }
@opts-setprefix url2md url2note
reify url2md
noglobfn url2md
function url2html() { url2note "$1" html }
@opts-setprefix url2html url2note
reify url2html
noglobfn url2html
##
function readmoz() {
    magic mdoc "[rmS= rmHtml= readmoz_nosummary=] $0 <url>
Outputs a summary of the URL and a cleaned HTML of the webpage to stdout. Set rmS to only print the summary." ; mret

    local url="$1"
    local summaryMode="$rmS"
    local noSummaryMode="${readmoz_nosummary:-$readmoz_ns}"

    local html
    html="${rmHtml:-$(full-html2 "$url")}" || {
        ecerr "${0}: Could not download $url; aborting."
        return 1
    }
    if ! ishtml-file =(ec "$html") ; then
        ecerr "$url is not an HTML file. Aborting."
        return 0 # this is not exactly an error. Returning 1 might cause useless retries.
    fi
    local cleanedhtml="$(<<<"$html" readability "$url")"
    if test -z "$noSummaryMode" ; then
        local prehtml="$(url2html "$url")"
        ec "$prehtml <hr> "
        # <p> --- </p>
    fi
    test -n "$summaryMode" || ec "$cleanedhtml"
}
noglobfn readmoz
function readmozsum() {
    : "Use url2html instead? No advtanges to this."
    rmS=y readmoz "$@"
}
noglobfn readmozsum
function readmozsum-file() {
    rmS=y readmoz-file "$@"
}
noglobfn readmozsum-file
function readmoz-file() {
    magic mdoc "$0 <file> [<url>]"
    local file="$1" url="${2:-https://${$(basename "$file"):-empty}.google.com}"
    local rmHtml="$(< "$file")"
    readmoz "$url"
}
noglobfn readmoz-file
function readmoz-mdold() {
    arcMode=oldest transformer to-archive-is readmoz-md "$@"
}
noglobfn readmoz-mdold
function readmoz-mdarc() {
    fhMode=curlfullshort transformer to-archive-is readmoz-md "$@"
}
noglobfn readmoz-mdarc
function readmoz-md() {
    local url="$1"
    local format=".${2:-md}"

    local md="$(gmktemp --suffix "$format")"
    # <() would not work with: readmoz-md https://github.com/google/python-fire/blob/master/docs/guide.md | cat
    # zsh sure is buggy :|
    pandoc -s -f html-native_divs =(readmoz "$url") -o $md
    < $md
    \rm $md
}
noglobfn readmoz-md
function readmoz-md2() {
    readmoz "$1" | html2text "${@:2}" # --ignore-links
}
noglobfn readmoz-md2
function readmoz-txt() {
    local opts=( "${@:2}" )
    test -n "$opts[*]" || opts=(--ignoreHref --ignoreImage --wordwrap=false --uppercaseHeadings=false --tables=true)
    readmoz "$1" | html-to-text "${opts[@]}" # returnDomByDefault
}
noglobfn readmoz-txt
function paulg() {
    getlinks http://www.paulgraham.com/articles.html | rg http://www.paulgraham.com | filter match-url | uniq -u
}
##
function mimetype2() {
    # TODO try using `github-linguist "$1"`, though it only works for text files.
    if isDarwin ; then
        # sometimes returns inaccurate results, e.g. identifying the readmoz of  https://www.greaterwrong.com/posts/L22jhyY9ocXQNLqyE/science-as-curiosity-stopper as text/plain
        file --brief --mime-type "$1"
    else
        # uses the extension for empty files, which is not desirable for us
        command mimetype --brief "$1"
    fi
}
function ishtml-file() {
    local mime="$(mimetype2 "$1")"
    # ^(text/(html|xml)|application/xhtml\+xml)$
    if ! [[ "$mime" =~ '(html|xml)' ]] ; then
        ecerr "$0: Mimetype of '$1' is '$mime'"
        return 1
    fi
}
function ishtml-link() {
    ishtml-file =(full-html2 "$1")
}
function getlinks-moz() {
    doc "You probably want to use getlinks-rec. This one is too low-level."
    
    local url="$1"
    getlinks.py "$url" =(readmoz "$url")
}
function getlinks-rec0() {
    local url="$1"
    # FNSWAP: getlinks-moz
    getlinks-moz "$url" | command rg --replace '$1' '([^#]*)(#.*)?' | command rg -v -e '.pdf$' -e '^(https?://)?(www.)?twitter.com'
}
function getlinks-rec-all() {
    fnswap getlinks-moz getlinks-c getlinks-rec "$@"
}
function getlinks-rec() {
    doc "$0 <url>
Gets 'useful' links from <url> recursively. (Depth=1, includes self)
outputs: <out::array>, stdout::newlineArray"

    local url="$1"
    local r1=( $url "${(@f)$(getlinks-rec0 "$url")}" )
    out=( "${(@u)r1}" )
    arrN "$out[@]"
}
function tlrec() {
    doc recursive tl

    local opts urls
    opts-urls "$@"
    if [[ "${#urls}" != 1 ]] ; then
        ecerr "tlrec's interface supports only a single URL. (${#urls} supplied.)"
        return 1
    fi
    getlinks-rec "$urls[1]" | inargsf tl $opts[@]
}
##
function getlinks-img() {
    gl_tag=img gl_prop=src getlinks-c "$@"
}
function urls-copy() {
    local text="$(cat)"
    <<<"$text" fnswap rg rgm match-url-rg --passthru && {
        local urls="$(<<<"${text}" urls-extract)"
        pbcopy "$urls"
    }
}
function urls-extract() {
    match-url-rg --only-matching --replace '$1'
}
function urls-cleansharps() {
    local urls="$(in-or-args "$@")"

    urls=( "${(@f)urls}" )
    local newUrls=()
    local url
    for url in $urls[@] ; do
        dvar url
        if [[ "$url" =~ '^(.*)\#.*$' ]] ; then
            dvar match
            newUrls+="$match[1]"
        else
          newUrls+="$url"
        fi
    done
    arrN ${(@u)newUrls}
}
noglobfn urls-cleansharps
aliasfn-ng urlc urls-cleansharps
##
function url-moddate() {
    : "Mostly useless because some sites don't have the header and others just set it incorrectly. Alt: url-date"

    local url="${1:?URL Required}"

    curlm --head "$url" | awk '/last-modified/{print}' | gcut -d ' ' -f2-
}
function url-goometa() {
    : "Usually contains the date."
    # https://stackoverflow.com/a/47037351/1410221

    local url="${1:?URL Required}"

    # `--time y19` makes it more likely that Google returns the date. We can't use a higher value than 19 years.
    local search="$(googler-en --time y19 --json --count "1" "$url")"
    # dact ec $search
    <<<$search jqm ' .[] | .metadata'
}
function url-date-wayback() {
    local url="$1"

    local first
    first="$(wayback-url "$url")" # || return 1

    # YYYYMMDDhhmmss
    if [[ "$first" =~ 'https://web.archive.org/web/(\d{4})(\d{2})(\d{2})\d*/' ]] ; then
        # gdate --date "$match[1]" "+%F"
        ec "${match[1]}-${match[2]}-${match[3]}"
    fi
}
function url-date() {
    local url="$1" date

    date="$(url-goometa "$url")" # Google's metadata can contain irrelevant stuff, but if they usually contain the date, and are more accurate than wayback's.
    if [[ "$date" =~ '^\s*$' ]] ; then
        date="$(url-date-wayback "$url")"
    fi
    ec $date
}
##
aliasfn rss-tll rss-tl -e w2e-curl
##
