# imports json.zsh
### Aliases
alias tlc='tlrl-code'
alias tlg='tlrl-gh'
###
function wread() {
    mdoc 'Out: wr_title wr_author' MAGIC
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
    wr_title="$(<<<"$merc" jqm -r .title)"
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
function full-html() {
	curlfull.js "$1" > "$2"
    #doc splash should be up. https://splash.readthedocs.io
    #doc 'wait always waits the full time. Should be strictly < timeout.'
    #curl --silent "http://localhost:8050/render.html?url=$1&timeout=90&wait=${fu_wait:-10}" -o "$2"
}
function random-poemist() {
    curl -s https://www.poemist.com/api/v1/randompoems |jq --raw-output '.[0].content'
}
xkcd() wget `wget -qO- dynamic.xkcd.com/comic/random | sed -n 's/Image URL.*: *\(\(https\?:\/\/\)\?\([\da-z\.-]\+\)\.\([a-z\.]\{2,6\}\)\([\/\w_\.-]*\)*\/\?\)/\1/p'`
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
wayback-url() {
    waybackpack --to-date "${wa_t:-2017}" --list "$@" |tail -n1
}
w2e-curl() {
    we_dler=wread-curl w2e "$@"
}
wread-curl() {
    gurl "$1"
}
w2e-gh() {
    h2ed=html2epub-pandoc-simple w2e-curl "$1" "${(@f)$(gh-to-readme "${@:2}")}"
}
gh-to-readme() {
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
gh-to-raw() rex 'rgx _ /blob/ /raw/' "$@"
url-final() {
    curl -Ls -o /dev/null -w %{url_effective} "$@" || ec "$@"
}
url-final2() {
	doc "This one doesn't download stuff."
	[[ "$(2>&1 wget --no-verbose --spider "$1" )" =~ '.* URL: (.*) 200 .*' ]] && ec "$match[1]" || url-final "$1"
}
reify url-final url-final2
url-tail() {
    [[ "$1" =~ '\/([^\/]+)\/?$' ]] && ec "$match[1]" || ec "$1"
}
function tlrlu(){
    tlrl-ng "$@" -p "$(url-tail "$(url-final "$1")") | "
}
tlrl-code(){
    w2e-code "$(url-tail "$(url-final "$1")")" "$@"
}
tlrl-gh() {
    w2e-gh "$(url-tail "$(url-final "$1")")" "$@"
}
tlrl-ng() {
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
    silent wread "$1" html || { ecerr "tlrl-ng: wread failed with $? on url $1" ; return 33 }
    local title="$( ec "${opts[-p]}${wr_title:-$1}" | sd / _ )"
    pushf "${opts[-o]:-$HOME/tmp-kindle}"
    we_author=$wr_author eval "$(gq "${opts[-e]:-w2e-raw}" "$title" "$@")"
    e=$?
    popf
    return $e
}
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
txt2epub() {
    mdoc "Usage: $0 <title> <authors> <txt-file>" MAGIC
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
    for url in "${(@f)$(urlfinalg "${@:2}")}"
    do
	    local bname="$(url-tail "$url")"  #"${url##*/}"
        #test -z "$bname" && bname="u$i"
        bname="${(l(${##})(0))i} $bname"
        i=$((i+1))

        retry-limited-eval "${we_retry:-10}" "${we_dler:-wread}" "$url:q" html '>' "$bname:q" && ec "Downloaded $url ..." || { ec "$url" >> failed_urls
                                                                                                                               ecerr "Failed $url"
                                                                                                                               hasFailed='Some urls failed (stored in failed_urls). Download them yourself and create the epub manually.'
            }
    done

    test -z "$hasFailed" && { ec "Converting to epub ..."
                              ecdbg files to send to h2ed *
                              html2epub "$1" "$author" * #.html
                              mv *.epub ../ && cd '../' && \rm -r "./$u"
                              ec "Book '$1' by '$author' has been converted successfully."
    } || { ecerr "$hasFailed" && (exit 1) }
}
w2e-raw() {
    web2epub "$1" "${@:2}" && p2k "$1.epub"
}
w2e-o() {
    wr_force=y w2e-raw "$1" "${(@f)$(outlinify "${@:2}")}"
}
w2e-lw-raw() {
    we_author=LessWrong w2e-curl "$1" "${(@f)$(re lw2gw "${@:2}")}"
}
lw2gw() rgx "$1" 'lesswrong\.com' greaterwrong.com
html2epub-pandoc-simple() {
    ecdbg "h2e-ps called with $@"
    pandoc --toc -s "${@:3}" --epub-metadata <(ec "<dc:title>$1</dc:title> <dc:creator> $2 </dc:creator>") -o "$1.epub"
}
aa2e() {
    ecerr DEPRECATED: Use w2e-curl.
    aget "aa -Z $(gquote "${@:2}")
html2epub-pandoc-simple $1:q ${${author:-aa2e}:q} *
mv $1:q.epub ../"
    p2k "$1".epub
}
code2html() {
    mdocu '[chroma-options] file -> HTML in stdout' MAGIC
    chroma --formatter html --html-lines --style trac "$@"
}
code2html-url() {
    code2html "${@:2}" <(gurl "$1")
}
wread-code() {
    code2html-url "$1"
}
w2e-code-old() {
    doc DEPRECATED: The html produced is not bad but after conversion we lose newlines which just sucks.
    we_dler=wread-code w2e "$1" "${(@f)$(gh-to-raw "${@:2}")}"
}
w2e-code() {
    mdocu '<name> <url> ...' MAGIC
    aget aa -Z "$(gquote "${(@f)$(gh-to-raw "${@:2}")}")" \; code2epub "$1:q" '*' \; mv ${1:q}.epub ../
    p2k ${1}.epub
}
code2md() {
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
merge-html() {
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
code2epub() {
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
	local u
	u="$(uuidgen)"
	full-html "$1" "$u"
	getlinks.py "$1" "$u" | command rg "${@[2,-1]:-.*}"
	\rm "$u"
}
noglobfn getlinksfull
function lwseq() {
	mdoc "Usage: [tl options] URL ...
	Creates an ebook out of the sequences specified." MAGIC
	local opts
    zparseopts -A opts -K -E -D -M -verbose+=v v+ -prefix-title:=p p: -engine:=e e: -outputdir:=o o:
	re lw2gw "$@" | inargsf re getlinksfull | command rg -F lesswrong.com/ | inargsf re lw2gw |inargsf tl -e "${opts[-e]}" -p "${opts[-p]}" -o "${opts[-o]}"
}
noglobfn lwseq
function urlfinalg() {
	doc supports Google redirects. Set uf_idem to y to return original.
	local URL="$1"
	test -n "$uf_idem" && {
	ec "$URL"
	return 0
	}
	local u="$URL"
	[[ "$URL" =~ "^(http(s)?://(www\.)?)?google\.com/.*" ]] && {
	u=`echo "$URL" | perl -n -e '/url=([a-zA-Z0-9%\.]*)/ && print "$1\n"'`
	u="$(ec $u | url-decode.py)"
	}
	url-final2 "$u"
}
reify urlfinalg
noglobfn urlfinalg
jwiki() {
    serr jwiki.py "$*" 1
}
wread-man() {
	local m=""
	m="$(MAN_KEEP_FORMATTING=1 COLUMNS=70 serr man "$1")" && m="$(<<<"$m" command ul)" || m="$(2>&1 "$1" --help)" || { ecerr "$0 failed for $1" ; return 1 }
	<<<"$m" aha --title "$1"
}
tlman() {
	uf_idem=y we_dler="wread-man" w2e "$1" "$@"
}
wread-bat() {
unbuffer bat --theme OneHalfLight --pager=never --style=plain "$1" | aha --title "$(basename "$1")"
}
tlbat() {
	uf_idem=y we_dler="wread-bat" w2e "$(basename "$1")" "$@"
}
