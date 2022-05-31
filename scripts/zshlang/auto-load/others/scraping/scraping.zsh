# imports json.zsh
##
export useragent_googlebot="Mozilla/5.0 (compatible; Googlebot/2.1; +http://google.com/bot.html)"

export useragent_chrome='Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/95.0.4638.54 Safari/537.36'
export useragent_header_chrome="User-Agent: ${useragent_chrome}"
### Aliases
alias withtxt="we_dler=readmoz-txt h2ed=txt2epub " # better for tables
alias tlcode='tlrl-code'
alias tlgh='tlrl-gh'
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
##
function curlm() {
    local nosilent="${curlm_ns}"

    local opts=()
    if test -z "$nosilent" ; then
        opts+='--silent'
    fi
    # cookie-jar saves cookies. I have it here to make curl activate its cookie engine.
    # --suppress-connect-headers is needed so that `curlm --head ...` requests are parseable.
    $proxyenv curl \
        --suppress-connect-headers \
        --header "$useragent_header_chrome" \
        --header "$(cookies)" \
        --fail \
        --location \
        --cookie-jar /dev/null \
        --styled-output \
        "$opts[@]" "$@"
}

function curl-dl() {
    ##
    # curl doesn't preallocate (see, e.g., https://curl.se/mail/archive-2014-02/0007.html)
    ##
    # cookies-auto takes ~0.5s
    # @api I rely on the cookies getting fetched here
    ##
    local curlm_ns=''
    isI && curlm_ns=y

    local opts=()
    opts+=( --header "$(cookies-auto "$@")" )

    curlm "$opts[@]" --remote-header-name --remote-name-all "$@"
    # remote-header-name: This  option tells the -O, --remote-name option to use the server-specified Content-Disposition filename instead of extracting a filename from the URL.
    # remote-name-all saves all URLs to their given names
    local r=$?
    if (( r == 23 )) ; then
        ecerr "$0: 23     Write error. Curl couldn't write data to a local filesystem or similar. Trying to use a random output name ..."
        ecerr "$0: Currently this is only supported for a single URL which should be the last arg"
        local url="${@[-1]}"
        local name=''
        name="$(url-filename "$url")" || name="$(url-tail "$url")"
        curlm "$opts[@]" --output "${name:r}_$(uuidm).${name:e}" "$@"
        return $?
    fi
    return $r
}
function curl-cookies() {
    # cookies-auto takes ~0.5s
    curlm --header "$(cookies-auto "$@")" "$@"
}
function curl-googlebot() {
    curlm --user-agent "$useragent_googlebot" "$@"
}
function curl-useragent() {
    # Copy from Chrome's network pane
    $proxyenv curl --fail --no-progress-meter \
        -H 'upgrade-insecure-requests: 1' \
        -H "$useragent_header_chrome" \
        -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' \
        -H 'accept-language: en-US,en;q=0.9,fa;q=0.8,ru;q=0.7,ur;q=0.6' \
        --compressed "$@"
}
##
function web-lastmod() {
    curlm -I "$1" 2>&1 | rg --smart-case last-modified
}
##
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
##
function httpm() {
    local c="$(cookies-auto "$@")"
    http --style solarized-light --follow --ignore-stdin --session "pink$(uuidpy)" "$@" $c
}
##
function full-html2() {
    # wget, aa, curl fail for https://www.fanfiction.net/s/11191235/133/Harry-Potter-and-the-Prince-of-Slytherin
    # seems to be because the server is messed up, but whatever:
    # http: error: Incomplete download: size=46696; downloaded=131310
    ##
    # assert-net @RET # not worth the perf hit?

    local url="$1"
    assert-args url @RET

    local absolutify="${fhAbs:-y}"

    local progress_p="${fhProgress}"
    #: currently only supported for 'aa'
    local progress_serr='dbgserr'
    if bool "$progress_p" ; then
        progress_serr='reval'
    fi

    local redirect_follow="${fhRedirect:-m}"
    if bool "$redirect_follow" ; then
        if [[ "$redirect_follow" == m ]] ; then
            #: Do only minimal processing of the URL.
            local urlfinalg_e1=cat
            local urlfinalg_e2=cat
        fi

        local tmp
        if tmp="$(fhRedirect=n urlfinalg "$url")" ; then
            url="$tmp"
        fi
    fi

    local mode="${fhMode}"
    if test -z "$mode" ; then
        if [[ "$url" =~ 'https?://[^/]+\.ir' ]] ; then
            mode='curl'
        else
            # mode='cloudscraper'
            mode='curl'
        fi
    fi

    local secure="${fhSecure:-y}" # insecure mode currently only supported by wget and gurl

    local opts=()

    local html
    html="$(
    if [[ "$url" =~ '^(?:https?://)?[^/]*techcrunch\.' ]] ; then
        ecdbg "$0: Techcrunch Mode"
        techcrunch-curl "$url"
        return $?
    elif [[ "$url" =~ '^(?:https?://)?[^/]*t\.co/' ]] ; then # techmeme sometimes links to FT using these
        ecdbg "$0: t.co Mode"
        mode='curlfull'
    fi

    [[ "$mode" =~ '^curlfullzero$' ]] &&  { cfTimeout=0 $proxyenv curlfull.js "$url" ; return $? } # not really usable
    [[ "$mode" =~ '^curlfullshorter$' ]] &&  { cfTimeout=0.5 $proxyenv curlfull.js "$url" ; return $? } # not really usable
    [[ "$mode" =~ '^curlfullshort$' ]] &&  { cfTimeout=1 $proxyenv curlfull.js "$url" ; return $? }
    [[ "$mode" =~ '^curlfull$' ]] && { cfTimeout=20 $proxyenv curlfull.js "$url" ; return $? }
    [[ "$mode" =~ '^curlfulllong$' ]] && { cfTimeout=900 $proxyenv curlfull.js "$url" ; return $? }
    [[ "$mode" =~ '^aa(cookies)?$' ]] && {
        local tmp="$(uuidgen)"
        local tmpdir="$(get-tmpdir)"
        aa_top_p="${aa_top_p:-n}" $proxyenv $progress_serr aacookies "$url" --dir "$tmpdir" -o "$tmp" || return $?
        < "$tmpdir/$tmp"
        return $?
        # Note that -o accepts basenames not paths which makes it incompatible with any /dev/* or other special shenanigans
    }

    [[ "$mode" =~ '^(c|g)url(fast)?$' ]] && {
        if ! bool $secure ; then
            opts+='--insecure'
        fi

        $proxyenv gurl "${opts[@]}" "$url"
        return $?
    }

    [[ "$mode" =~ '^wgetm?$' ]] && {
        if ! bool $secure ; then
            opts+='--no-check-certificate'
        fi

        $proxyenv wgetm -O - "${opts[@]}" "$url"
        return $?
    }

    [[ "$mode" =~ '^cloudscraper$' ]] && {
        # @upstreamBug Corrupts =Python’s features= to =Pythonâs features=
        $proxyenv cloudscraper_get.py "$url" # idk if proxyenv works for this
        return $?
    }

    [[ "$mode" =~ '^http(ie)?$' ]] && {
        $proxyenv dbgserr httpm "$url"
        return $?
    }
    )" @RET

    if bool "$absolutify" && ishtml-file =(ec "$html") ; then
        ec "$html" | html-links-absolutify "$url"
    else
        ec "$html"
    fi
}
aliasfn html-get full-html2

function full-html() {
    local url="$1" dest="$2"
    assert-args url dest @RET

    local content
    content="$(fhMode="${fhMode:-curlfull}" full-html2 "$url")" @RET
    ec "$content" > "$dest"
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
##
function w2e-curl() {
    we_dler=wread-curl w2e "$@"
}
noglobfn w2e-curl

function wread-curl() {
    local url="$1"
    assert-args url @RET

    full-html2 "$url"
}
##
function html2epub-calibre() {
    mdoc "Usage: $0 <title> <authors> <html-file> ..." MAGIC
    local authors="${2:-night}"
    local title="$1"
    title="$(ec "$title" | str2pandoc-title)" || true

    local u
    u="$title $(uuidgen).html" @TRET

    merge-html "${@:3}" > "$u" @TRET
    ebook-convert "$u" "$title.epub" \
        --authors="$authors" \
        --level1-toc="//*[name()='h1' or name()='h2']" \
        --level2-toc="//h:h3" \
        --level3-toc="//*[@class='subsection']" \
        --page-breaks-before="//*[(name()='h1' or name()='h2') or @class='owner-name']" \
        --use-auto-toc --toc-threshold=0 \
        --toc-title="The TOC" \
        --embed-all-fonts \
        --title="$title" --epub-inline-toc --enable-heuristics >&2 @RET

    silent trs-rm "$u"

    ec "${title}.epub"
}
##
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

function t2e() {
    local title="$1"
    title="$(ec "$title" | str2pandoc-title)"  || true

    txt2epub "$title" "${te_author:-night_t2e}" "${@:2}"
    p2k "$title".epub
}

function html2epub() {
    local files=( "$@[3,-1]" ) engine="${h2ed:-html2epub-calibre}"

    # filter0 ishtml-file # don't do this, as we download non-html files in w2e-curl
    arr0 "$files[@]" | inargs0 revaldbg "${engine}" "$1" "$2"
}

function html2epub-pandoc() {
    # usage: title author html ...
    ##
    local title="$1"
    title="$(ec "$title" | str2pandoc-title)" || true
    local author="$2" htmls=("${@:3}")
    assert-args title htmls @RET

    PANDOC_FORMAT=html-native_divs 2epub-pandoc-byformat "$title" "$author" <(merge-html "$htmls[@]") >&2

    ec "${title}.epub"
}

function h2e() {
    local out
    if out="$(html2epub "$1" "${h2_author:-night}" "${@:2}")" ; then
        ec "Book '${out:t}' created."

        p2k "$out"
    else
        ecerr "$0 failed for book: $1"
        return 1
    fi
}
##
function web2epub() {
    doc usage: 'we_strict= we_retry= we_dler= we_author= title urls-in-order'

    local title="$1"
    title="$(ec "$title" | str2pandoc-title)"  || true
    local u="$title $(uuidgen)"
    cdm "$u"
    local author="$(<<<${we_author:-night} str2pandoc-title)"
    local i=0
    local hasFailed=''
    local strict="$we_strict"

    local dled_files=()

    local urls=("${@:2}")
    assert-args urls @RET
    #: @fnswap urlfinalg
    urls=("${(@f)$(urlfinalg $urls[@])}") @TRET

    for url in $urls[@]
    do
        local bname="$(url-tail "$url")"  #"${url##*/}"
        #test -z "$bname" && bname="u$i"
        bname="${(l(${##})(0))i} ${bname}" # Do NOT add an extension here, so that w2e-curl works.
        i=$((i+1))

        # API Change; old: "${we_dler:-wread}"
        if retry-limited-eval "${we_retry:-10}" "${we_dler:-readmoz}" "$url:q" '>' "$bname:q" ; then
            ec "Downloaded $url ..."
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

        return 1
    else
        ec "Converting to epub ..."
        revaldbg html2epub "$title" "$author" "$dled_files[@]"
        mv *.epub ../ && cd '../' &&  { { isDbg || test -n "$hasFailed" } || \rm -r "./$u" }
        ec $'\n\n'"Book '$title' by '$author' has been converted (hasFailed='${hasFailed}')."$'\n\n'
    fi
}

function str2pandoc-title {
    local title
    title="$(in-or-args "$@")"
    title="$(ec "$title" | gtr '/' '.' | sd '&' ' and ')" || true

    ec "$title"
}

function w2e-raw() {
    local title="$1"
    title="$(ec "$title" | str2pandoc-title)" || true
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
##
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

    local title
    title="$(<<<$1 str2pandoc-title)" @TRET
    local author
    author="$(<<<$2 str2pandoc-title)" @TRET
    local dest="${pandoc_convert_dest:-$title.epub}"

    pandoc --toc -s "${@:3}" --metadata title="$title" --epub-metadata <(ec "<dc:title>$title</dc:title> <dc:creator> $author </dc:creator>") -o "$dest" >&2

    grealpath -e "$dest"
}
@opts-setprefix 2epub-pandoc-simple pandoc_convert
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
        local t
        t="$(gmktemp --suffix ."$ext")" @TRET
        cp "$i" "$t" >&2 @TRET
        txts+="$t"
    done
    2epub-pandoc-simple "$title" "$author" "$txts[@]"
}

aliasfn txt2epub-pandoc PANDOC_EXT=txt 2epub-pandoc-byext

aliasfn md2epub-pandoc PANDOC_EXT=md 2epub-pandoc-byext
##
aliasfn org2epub-pandoc PANDOC_EXT=org 2epub-pandoc-byext
# function org2epub() {
#     @opts from org to epub @ pandoc-convert "$@"
# }
function org2epub {
    : "you probably want 'org2epub-auto-metadata' instead"

    org2epub-pandoc "$@"
}

function org2epub-auto-metadata {
    local org_file="$1" url="${org2epub_url}" author="${org2epub_author}"
    assert-args org_file @RET
    local title
    title="${org_file:t:r}"
    title="$(ec "$title" | str2pandoc-title)" @TRET
    assert-args title @RET
    local reading_est
    reading_est="$(cat "$org_file" | count-words-humanfriendly)" @STRUE
    local url_date=''
    if test -n "$url" ; then
        url_date="$(url-date "$url")" @STRUE
    fi

    local epub_file
    epub_file="${org_file:h}/${title}.epub"
    epub_file="$(reval-env-ec pandoc_convert_dest="${epub_file}" org2epub-pandoc "$title" "[${reading_est}] ${author} $url_date" "$org_file")" @TRET

    epub_file="$(reval-ec p2k "${epub_file}")" @TRET

    grealpath -e "$epub_file"
}
@opts-setprefix org2epub-auto-metadata org2epub
##
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
##
function merge-html() {
    {
        if (( $# == 1 )) ; then
            ec "<p>$(html-get-reading-estimate $1)</p>"$'\n' @STRUE
            cat $1 @TRET
        else
            local i
            for i in "$@"
            do
                ec "<h1>$(strip $i ".html") - $(html-get-reading-estimate $i)</h1>"$'\n' @STRUE
                cat "$i" @TRET
            done
        fi
    } | html-links-textualize
}
##
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

    local url="$1" query="${@[2,-1]:-.*}"

    local u
    u="$(uuidpy)" @TRET
    full-html "$url" "$u" @RET
    test -e "$u" || { ecerr "${0}: Couldn't download $url" ; return 1 }
    getlinks.py "$url" "$u" | trimsed | command rg "$query"

    silent trs-rm "$u"
}
noglobfn getlinksfull

function p-getlinks {
    local url="${1}"
    if test -z "$url" ; then
        url="$(browser-current-url | sd '#$' '')" @TRET
    fi
    assert-args url @RET

    local html
    html="$(pbpaste-html)" || true
    if test -z "$html" ; then
        html="$(pbpaste)" @TRET
    fi
    assert not isSpace html @RET

    getlinks.py "$url" =(ec "$html")
}
##
function getlinksfull2() {
    mdoc "$0 [-e,--regex <flitering-regex>] <url> ...
Uses getlinksfull (full-html) under the hood." MAGIC
    local opts
    zparseopts -A opts -K -E -D -M -regex:=e e:
    local pages=("$@")
    local regex="${opts[-e]:-.*}"

    for page in "$pages[@]"
    do
        getlinksfull "$page" "$regex" || true # errors are ignored
    done
}

function getlinks2() {
    fhMode="${fhMode:-curl}" getlinksfull2 "$@"
}

function getlinks-c() {
    # @hiddenAPI 'fhMode=aacookies' is used by aamedia to fnswap aria2c
    fhMode="${fhMode:-aacookies}" getlinksfull2 "$@"
}

function getlinks-uniq() {
    getlinks-c "$@" | gsort --unique
}
##
function http-headers-to-json() {
    http_headers_to_json.py
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
        if isDbg ; then
            ecerr "$0: Mimetype of '$1' is '$mime'"
        fi
        return 1
    fi
}

function ishtml-link() {
    ishtml-file =(full-html2 "$1")
}
##
function getlinks-moz() {
    doc "You probably want to use getlinks-rec. This one is too low-level."
    
    local url="$1"
    getlinks.py "$url" =(readmoz "$url")
}

function getlinks-rec0() {
    local url="$1"
    # @fnswap getlinks-moz
    getlinks-moz "$url" | urls-cleansharps | command rg -v -e '.pdf$' -e '^(https?://)?(www.)?twitter.com'
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
    out=( ${(@u)r1} )
    arrNN "$out[@]"
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
##
aliasfn rss-tll rss-tl -e w2e-curl
##
aliasfn html-links-textualize ifdefined-cmd-or-cat html_links_textualize.lisp

function html-links-absolutify {
    local url_current="$1"
    assert-args url_current @RET
    local url_root
    url_root="${2:-$(url-head "$url_current")}" @TRET

    ifdefined-cmd-or-cat html_links_absolutify.lisp "$url_current" "$url_root"
}
##
