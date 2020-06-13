# imports json.zsh
### Aliases
alias withtxt="we_dler=readmoz-txt h2ed=txt2epub " # better for tables
alias tlcode='tlrl-code'
alias tlgh='tlrl-gh'
alias gurl='curlm -o /dev/stdout'
alias wread-c='fhMode=curl wr_force=y wread'
alias withchrome='fhMode=curlfullshort '
alias w2e-chrome='withchrome fnswap urlfinalg arrN w2e' # readmoz uses full-html2 under the hood.
alias tlchrome='tlrl-ng -e w2e-chrome'
aliasfn tlf tlchrome
alias w2e-curl-wayback='we_dler=wread-curl w2e-wayback'
###
function tlarc() {
  fhMode=aa transformer to-archive-is tl "$@" 
}
function to-archive-is() {
  # will fail if no snapshots exist
  print -r -- "http://archive.is/newest/$(urlfinalg "$1")"
  # oldest is also supported
}
function tllwb() {
    # we can't use an alias because tl won't get the correct URLs then.
    tll "${(@f)$(wayback-url "${@}")}"
}
noglobfn tllwb

function wgetm() {
    wget --header "$(cookies-auto "$@")" "$@"
}
function curlm() {
    curl --silent --fail --location --cookie-jar "$(mktemp)" --header "$(cookies-auto "$@")" "$@"
}
getcookies() {
    mdoc "[cookiesFile= ] $0 <url>
Will output Chrome's cookies for the given URL in key=val;key2=val2
See |cookies| for higher-level API." MAGIC
    local url="$1"
    local cf="${cookiesFiles:-${HOME}/Library/Application Support/Google/Chrome/Default/Cookies}"

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
                  fu_wait="${fu_wait:-60}" aget "full-html $1:q ./a.html
# l
# cat ./a.html
mercury-html $1:q ./a.html $2:q"
              } } || {
          # File supplied
          aget "cat ${(q@)file} > a.html ; mercury-html $1:q ./a.html $2:q"
      } })"
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
    http --style solarized-light --ignore-stdin --session "pink$(uuidpy)" "$@" "$(cookies-auto "$@")"
}
function full-html2() {
    # wget, aa, curl fail for https://www.fanfiction.net/s/11191235/133/Harry-Potter-and-the-Prince-of-Slytherin
    # seems to be because the server is messed up, but whatever:
    # http: error: Incomplete download: size=46696; downloaded=131310

    #fhMode="${fhMode:-http}" full-html "$1" /dev/stdout

    local mode="${fhMode:-http}"
    local stdout=/dev/stdout
    [[ "$mode" =~ '^curlfullshort$' ]] &&  { cfTimeout=1 curlfull.js "$1" ; return $? }
    [[ "$mode" =~ '^curlfull$' ]] && { cfTimeout=20 curlfull.js "$1" ; return $? }
    [[ "$mode" =~ '^curlfulllong$' ]] && { cfTimeout=900 curlfull.js "$1" ; return $? }
    [[ "$mode" =~ '^aa(cookies)?$' ]] && {
        local tmp="$(uuidgen)"
        local tmpdir="$(get-tmpdir)"
        dbgserr aacookies "$1" --dir "$tmpdir" -o "$tmp" || return $?
        < "$tmpdir/$tmp"
        return $?
        # Note that -o accepts basenames not paths which makes it incompatible with any /dev/* or other special shenanigans
    }
    [[ "$mode" =~ '^(c|g)url$' ]] && { gurl "$1" ; return $? }
    [[ "$mode" =~ '^http(ie)?$' ]] && {
        dbgserr httpm "$1"
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
    waybackpack --to-date "${wa_t:-2017}" --list "$@" |tail -n1
}
enh-urlfinal wayback-url
reify wayback-url
noglobfn wayback-url
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
function gh-to-readme() {
    local urls=() i i2 readme url
    for i in "$@"
    do
        ! [[ "$i" =~ 'github.com' ]] || [[ "$i" == *.(md|rst) ]] ||
            {    i2="${i}.md"
                 comment we hope to handle wiki pages with method, but beware that nonexistent wiki pages trigger create a new page, not the desired not existent response.
                 url-exists "$i2" ||
                     { i2="${i}/blob/master/README.md"
                       url-exists "$i2" ||
                           { i2="${i}/blob/master/README.rst"
                             url-exists "$i2" ||
                                 { i2="${i}/blob/master/readme.md"
                                   url-exists "$i2" ||
                                       { i2="${i}/blob/master/readme.rst"
                                         url-exists "$i2" ||
                                             {
                                                 for readme in "${(0@)$(permute-case readme)}"
                                                 do
                                                     i2="${i}/blob/master/${readme}.md"
                                                     url-exists "$i2" && break
                                                     i2="${i}/blob/master/${readme}.rst"
                                                     url-exists "$i2" && break
                                                 done
                                             } } } } }
                 i="$i2"
            }
        url-exists "$i" && urls+="$i" || color red "$i does not seem to exist." >&2
    done
    gh-to-raw "$urls[@]"
}
function gh-to-raw() rex 'rgx _ /blob/ /raw/' "$@"
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
    ec "$(urlmeta "$1" title) $(url-tail "$(urlfinalg "$1")")"
}
renog url-tailedtitle
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
        author="[$readest] $author"
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
    local u="$1 $(uuidgen).html" title="${1}" authors="${2:-nHight}"
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
txt2epub-pandoc () {
# TODO turn this into general pandoc convertor by modularizing the suffix
    magic mdocu "<title> <author> <file> ..." ; mret
    local files=( "${@:3}" )
    local txts=()
    local i
    for i in "$files[@]" ; do
      local t="$(gmktemp --suffix .txt)"
      cp "$i" "$t"
      txts+="$t"
    done
    pandoc --toc -s "$txts[@]" --epub-metadata <(ec "<dc:title>$1</dc:title> <dc:creator> $2 </dc:creator>") -o "$1.epub"
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
html2epub() {
    ecdbg calling "${h2ed:-html2epub-calibre}" "$@"
    "${h2ed:-html2epub-calibre}" "$@"
}
html2epub-pandoc() {
    # title author htmls
    pandoc --toc -s -f html-native_divs <(merge-html "${@:3}") --epub-metadata <(ec "<dc:title>$1</dc:title> <dc:creator> $2 </dc:creator>") -o "$1.epub"
}
h2e() {
    html2epub "$1" "${h2_author:-night}" "${@:2}"
    p2k "$1".epub
}
web2epub() {
    doc usage: 'we_retry= we_dler= we_author= title urls-in-order'
    local u="$1 $(uuidgen)"
    cdm "$u"
    local author="${we_author:-night}"
    local i=0
    local hasFailed=''
    # FNSWAP: urlfinalg
    for url in "${(@f)$(urlfinalg "${@:2}")}"
    do
        local bname="$(url-tail "$url")"  #"${url##*/}"
        #test -z "$bname" && bname="u$i"
        bname="${(l(${##})(0))i} $bname"
        i=$((i+1))

        # API Change; old: "${we_dler:-wread}"
        retry-limited-eval "${we_retry:-10}" "${we_dler:-readmoz}" "$url:q" '>' "$bname:q" && ec "Downloaded $url ..." || {
                ec "$url" >> failed_urls
                ecerr "Failed $url"
                hasFailed='Some urls failed (stored in failed_urls). Download them yourself and create the epub manually.'
            }
    done

    test -z "$hasFailed" && { ec "Converting to epub ..."
                              ecdbg files to send to h2ed *
                              html2epub "$1" "$author" * #.html
                              mv *.epub ../ && cd '../' &&  { isDbg || \rm -r "./$u" }
                              ec "Book '$1' by '$author' has been converted successfully."
    } || { ecerr "$hasFailed" && (exit 1) }
}
function w2e-raw() {
    web2epub "$1" "${@:2}" && p2k "$1.epub"
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

function html2epub-pandoc-simple() {
    ecdbg "h2e-ps called with $@"
    pandoc --toc -s "${@:3}" --epub-metadata <(ec "<dc:title>$1</dc:title> <dc:creator> $2 </dc:creator>") -o "$1.epub"
}
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
function code2epub() {
    mdoc "Usage: we_author=<author> $0 <title> <sourcecode> ..." MAGIC
    pandoc -s --epub-metadata <(ec "<dc:title>$1</dc:title> <dc:creator> ${we_author:-night} </dc:creator>") -f markdown <(code2md "${@[2,-1]}") -o "${1}.epub"
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
    getlinks.py "$1" "$u" | command rg "${@[2,-1]:-.*}"
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
    url-final2 "$u"
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
    fhMode=curl getlinksfull2 "$@"
}
function getlinks-c() {
    fhMode=aacookies getlinksfull2 "$@"
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
function urlmeta() {
    mdoc "[html= ] $0 <url> <req>
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
function hi10-ng() {
    mdoc "$0 <url-of-hi10-page> [<regex>]" MAGIC
    local url="$1"
    local regex=${2:-'\.mkv$'}

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
    getlinks-c -e '\.[^/]+$' "$urls[@]"
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
    { test -z "$lgNoBok" && libgendl-md5-bok "$md5" } || {
        ecerr "bok failed. Trying main ..."
        libgendl-md5-main "$md5" | inargsf aa -Z
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
    jlibplain "$@"
    dir2k
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
    magic mdoc "[ html= cleanedhtml= ] $0 <url> [<mode>] ; outputs in global variables and stdout." ; mret

    local url="$(urlfinalg "$1")"
    local mode="${2:-md}"

    local html="${html:-$(full-html2 "$url")}"
    local cleanedhtml="${cleanedhtml:-$(<<<"$html" readability "$url")}"
    
    meta=( "${(@0)$(urlmeta $url all)}" )
    title="$meta[1]"
    desc="$meta[2]"
    img="$meta[3]"
    author="$meta[4]"
    local indent="    "
    readest="$(<<<"$cleanedhtml" html-get-reading-estimate /dev/stdin)"

    if [[ "$mode" == md ]] ; then
        ec "* [${title:-$url}]($url)"
        test -z "$author" || ec "${indent}* By: $author"
        test -z "$readest" || ec "${indent}* $readest"
        test -z "$desc" || ec "${indent}* $desc"
        #test -z "$title" || ec "${indent}* $url"
        test -z "$img" || ec '![]'"($img)"
    elif [[ "$mode" == html ]] ; then
        test -z "$title" || ec "<h1>${title}</h1>"
        test -z "$author" || ec "<p>By: $author</p>"
        test -z "$readest" || ec "<p>${readest}</p>"
        test -z "$desc" || ec "<p>Description: $desc</p>"
        test -z "$img" || ec "<img src=\"$img\" />"
    elif [[ "$mode" == none ]] ; then

    fi

}
function url2md() { url2note "$1" md }
function url2html() { url2note "$1" html }
reify url2md
reify url2html
noglobfn url2note
noglobfn url2md
noglobfn url2html
##
function readmoz() {
    magic mdoc "[rmS= rmHtml= ] $0 <url>
Outputs a summary of the URL and a cleaned HTML of the webpage to stdout. Set rmS to only print the summary." ; mret

    local url="$1"
    local summaryMode="$rmS"

    local html
    html="${rmHtml:-$(full-html2 "$url")}" || {
        ecerr "${0}: Could not download $url; aborting."
        return 1
    }
    local cleanedhtml="$(<<<"$html" readability "$url")"
    local prehtml="$(url2html "$url")"
    ec "$prehtml"
    # <p> --- </p>
    test -n "$summaryMode" || ec "<hr> $cleanedhtml"
}
function readmozsum() {
    : "Use url2html instead? No advtanges to this."
    rmS=y readmoz "$@"
}
function readmozsum-file() {
    rmS=y readmoz-file "$@"
}
function readmoz-file() {
    magic mdoc "$0 <file> [<url>]"
    local file="$1" url="${2:-https://${$(basename "$file"):-empty}.google.com}"
    local rmHtml="$(< "$file")"
    readmoz "$url"
}
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
function readmoz-md2() {
    readmoz "$1" | html2text "${@:2}" # --ignore-links
}
function readmoz-txt() {
    local opts=( "${@:2}" )
    test -n "$opts[*]" || opts=(--ignoreHref --ignoreImage --wordwrap=false --uppercaseHeadings=false --tables=true)
    readmoz "$1" | html-to-text "${opts[@]}" # returnDomByDefault
}
function paulg() {
  getlinks http://www.paulgraham.com/articles.html | rg http://www.paulgraham.com | filter match-url | uniq -u 
}
