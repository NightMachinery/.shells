##
function url-normalize() {
    local l="${1:?}"

    l_norm="$(url_normalizer.js "$l")" || l_norm="$l"
    ec  "$l_norm"
}
renog url-normalize
##
function url-tailf() {
    ec "$(url-tail "$(urlfinalg "$1")")"
}

function url-tailedtitle() {
    ec "$(url-title "$1") $(url-tailf "$1")"
}
renog url-tailedtitle

function url-title() {
    : "See also url-filename"

    local urls
    urls=(${(@f)"$(in-or-args "$@")"}) @RET

    local url
    for url in "${urls[@]}" ; do
        urlmeta2 "${url}" title @TRET
    done
}
noglobfn url-title
##
function match-url-rg() {
    # FNSWAP: rg
    rg --engine pcre2 -e "$nightUrlRegex" "$@"
}
aliasfn url-match-rg match-url-rg

function match-url2() {
    ec "$*" | ghead -n 1 | silent match-url-rg
}
function match-url() {
    # sometimes errs, presumably because it runs out of memory
    [[ "$*" =~ "^$nightUrlRegex\$" ]]
}
function match-url-liberal() {
    # https://gist.github.com/gruber/249502
    doc "Doesn't require http"

    [[ "$*" =~ '(?i)\b((?:[a-z][\w-]+:(?:/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'\''".,<>?«»“”‘’]))' ]]
}

aliasfn url-match match-url2
##
function url-sha256() {
    brishzr l_url-sha256 "$@"
}
noglobfn url-sha256

function l_url-sha256() {
    local url="$1"
    assert-args url @RET

    gurl "$url" | sha256sum | awkn 1 @TRET
}
renog l_url-sha256
##
function url-encode() {
    local inargs
    in-or-args2 "$@"

    ecn "$inargs" | url-encode.py
}
function url-decode() {
    local inargs
    in-or-args2 "$@"

    ecn "$inargs" | url-decode.py
}
##
##
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
    ##
    # The most reliable and expensive way.
    # it was too expensive, so @deprecated
    #
    # retry-limited 3 urlfinal.js "$1" || url-final2 "$1"
    ##
    url-final2 "$1"
}
reify url-final url-final2 url-final3
noglobfn url-final url-final2 url-final3
##
function url-tail() {
    [[ "$1" =~ '\/([^\/]+)\/?$' ]] && ec "$match[1]" || ec "$1"
}
reify url-tail
noglobfn url-tail

typeset -g url_head_regex='(?i)\b(https?:/{1,3}[^/]+[.][^/]+)'
function url-head() {
    local url="$1"
    assert-args url @RET

    if [[ "$url" =~ "$url_head_regex" ]] ; then
        ec "${match[1]}"
    else
        ec "$url"
    fi
}
reify url-head
noglobfn url-head
##
function url-exists() {
    local ret=1 url="$1"

    # Don't use --head, it doesn't work with some urls, e.g., https://github.com/Radarr/Radarr/wiki/Setup-Guide.md . Use `-r 0.0` to request only the first byte of the file.
    curl --output /dev/null --silent -r 0-0 --fail --location "$url" && {
        ret=0
        url_exists_out="${url_exists_out:-$url}" # only set it if it's not set before. This helps us try  a bunch of URLs and find the first one that exists.
    }
    return "$ret"
}
##
function url-clean-unalix() {
    # does url-clean-google itself
    local redirects="${url_clean_redirects}"
    local inargs
    in-or-args2 "$@" @RET

    if test -n "$uf_idem" ; then
        arrN $inargs[@]
        return $?
    fi

    local opts=()
    if bool $redirects ; then
        opts+='--unshort'
    fi

    {
        arrN $inargs[@] | { url-match-rg || true } | unalix --disable-certificate-validation "$opts[@]" @TRET
        ec
        arrN $inargs[@] | url-match-rg -v || true
    } | prefixer --skip-empty | enl

    ## tests:
    # `pop | { tee /dev/tty ; ec '======' > /dev/tty } | unalix`
    #
    # `echo 'https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwiY2ZrqxsHwAhXhB2MBHda6BUsQFjAJegQIBBAD&url=https%3A%2F%2Fapps.apple.com%2Fus%2Fapp%2Finspect-browser%2Fid1203594958&usg=AOvVaw2O_zES4FcNiKDn0veAc1bM' | unalix`
    #
    # `echo 'https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwiY2ZrqxsHwAhXhB2MBHda6BUsQFjAJegQIBBAD&url=https%3A%2F%2Fapps.apple.com%2Fus%2Fapp%2Finspect-browser%2Fid1203594958&usg=AOvVaw2O_zES4FcNiKDn0veAc1bM'$'\n''https://www.imdb.com/title/tt7979580/?pf_rd_m=A2FGELUUNOQJNL&pf_rd_p=ea4e08e1-c8a3-47b5-ac3a-75026647c16e&pf_rd_r=J6DRF89QKAFZ76S5FZYE&pf_rd_s=center-1&pf_rd_t=15506&pf_rd_i=moviemeter&ref_=chtmvm_tt_1'$'\n''https://bitly.is/Pricing-Pop-Up' | { tee /dev/tty ; echo '======' > /dev/tty } | @opts redirects y @ url-clean-unalix`
    #
    # `@opts redirects y @ url-clean-unalix 'https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwiY2ZrqxsHwAhXhB2MBHda6BUsQFjAJegQIBBAD&url=https%3A%2F%2Fapps.apple.com%2Fus%2Fapp%2Finspect-browser%2Fid1203594958&usg=AOvVaw2O_zES4FcNiKDn0veAc1bM' 'https://www.imdb.com/title/tt7979580/?pf_rd_m=A2FGELUUNOQJNL&pf_rd_p=ea4e08e1-c8a3-47b5-ac3a-75026647c16e&pf_rd_r=J6DRF89QKAFZ76S5FZYE&pf_rd_s=center-1&pf_rd_t=15506&pf_rd_i=moviemeter&ref_=chtmvm_tt_1' 'https://bitly.is/Pricing-Pop-Up'`
    ##
}
@opts-setprefix url-clean-unalix url-clean
aliasfn url-clean url-clean-unalix
noglobfn url-clean

function url-clean-google() {
    # @alt url-clean
    ##
    local URL="$1"
    assert-args URL @RET

    local u="$URL"
    [[ "$URL" =~ "^(http(s)?://(www\.)?)?google\.com/.*" ]] && {
        [[ "$URL" =~ "url=([^&]*)" ]] && u="$match[1]" || {
                ecerr "$0: failed to decode Google url $(gq "$URL")"
                return 1
            }

        u="$(ec $u | url-decode.py)"
    }

    ec "$u"
}
renog url-clean-google

function h-urlfinalg2() {
    @opts redirects y @ url-clean "$@"
}
aliasfn urlfinalg2 urlfinalg_e1=h-urlfinalg2 urlfinalg_e2=cat urlfinalg
noglobfn urlfinalg2

function h-urlfinalg1() {
    local inargs
    inargs="$(in-or-args "$@")" @RET
    inargs=(${(@f)inargs})

    local URL
    for URL in $inargs[@] ; do
        url-final "$u" @TRET #url-final2 sometimes edits URLs in bad ways, while url-final downloads them.
    done
}
aliasfn urlfinalg1 urlfinalg_e1=h-urlfinalg1 urlfinalg_e2=cat urlfinalg
noglobfn urlfinalg1

function urlfinalg {
    local print_p="${urlfinalg_print:-y}"
    local engine1=("${urlfinalg_e1[@]:-h-urlfinalg2}")
    local engine2=("${urlfinalg_e2[@]:-h-urlfinalg1}")
    local inargs disabled="${uf_idem}"
    inargs="$(in-or-args "$@")" @RET

    if bool "$disabled" ; then
        ec "$inargs"
        return 0
    fi

    local res url tmp
    for url in ${(@f)inargs} ; do
        if ! match-url "$url" || [[ "$url" == *bloomberg.com* ]] ; then
            ec "$url"
            continue
        fi

        url="$(url-clean-google "$url")" @TRET

        if ! res="$(ec "$url" | reval "$engine1[@]")" ; then
            ecerr "$0: $(gq "$engine1[@]") failed. Retrying with $(gq "$engine2[@]") ..."

            res="$(ec "$url" | reval "$engine2[@]")" @TRET
        fi
        url="$res"

        if bool "$print_p" ; then
            if [[ "$url" =~ '^https?://www.anandtech.com/.*' ]] ; then
                if tmp="$(full-html2 "${url}" \
                    | htmlq -a href '.print_article' \
                    | rg '/print/' )" ; then
                    url="$tmp"
                else
                    ecgray "$0: Could not get the print version of the URL $(gquote-dq "$url") (IGNORED)"
                fi
            elif [[ "$url" =~ '^https?://(?:[^/]*\.)?wikipedia\.org/+(?:wiki/+)?([^/]*)(/+.*)?' ]] ; then
                if true ; then
                    local title="${match[1]}"
                    # title="$(ecn $title | url-decode | url-encode)" #: not needed, the title should already be URL-encoded in the first place
                    url="https://en.wikipedia.org/w/index.php?title=${title}&printable=yes"
                else
                    #: The print link is no longer present in the scraped HTML (though it is still available in Chrome), IDK why.
                    if tmp="$(full-html2 "${url}" \
                        | htmlq -a href '[accesskey="p"]' \
                        | rg '&printable=yes' )" ; then
                        url="$tmp"
                    else
                        ecgray "$0: Could not get the print version of the URL $(gquote-dq "$url") (IGNORED)"
                    fi
                fi
            fi
        fi

        ec "$url"
    done
}
aliasfn url-final-gateway urlfinalg
noglobfn urlfinalg
##
function url-filename() {
    : "works with multiple URLs already"
    local python_parser="${url_filename_p}"

    curlm --head "$@" | \
        {
            if bool "$python_parser" ; then
                http-headers-to-json | jqm '."content-disposition"[0].filename'
            else
                @opts r '$1$2' @ rget \
                    'content-disposition:.*filename=\s*(?:"(.*)"|(.*))'
            fi
        }
}

function url-size() {
    local size
    size="$(curlm --head "$@" | rget '^content-length\S*\s*(\d+)' | gtail -n 1)" || return $?
    # if redirects are present a URL can have multiple content-lengths, hence the tailing

    # test -z "$size" && return 1 # rget ensures it
    if isOutTty ; then
        ec "$size" | numfmt-humanfriendly-bytes
    else
        ec "$size"
    fi
}
##
function urlmeta2() {
    mdoc "[html= ] $0 <url> <req> ...
gets the requested metadata. If html is supplied, will use that. In that case, <url> is superfluous." MAGIC

    local url="$1"

    # local fhMode="${fhMode:-curl}"

    local html
    html="${html}"
    if test -z "$html" ; then
        html=$(fhSecure="${fhSecure:-n}" full-html2 "$url") || {
            local msg="$0: full-html2 failed; fhMode: ${fhMode}, URL: $(gq "$url"), retcode: $?"
            ectrace "$msg"

            # return 1
            true # when we encounter, e.g., captchas, usually the =meta= tags we need can be downloaded without problems; So ignoring the error seems the best tradeoff.
        }
    fi
    local reqs=("${@:2}")
    <<<"$html" assert htmlmetadata $reqs[@] || {
        local tmp="$(gmktemp --suffix .html)"
        ec "$html" > $tmp
        ecerr "$0: Copied input HTML to $(gq "$tmp")"
        return 1
    }
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
    silent trs-rm "$f"
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

    url="$(url-clean "$url")"
    # url="$(urlfinalg "$url")"

    local imgMode="${url2note_img}" emacsMode="${url2note_emacs}"

    if [[ "$url" =~ '^(?:https?://)?[^/]*youtube.com' ]] ; then
        if bool "$emacsMode" ; then
            imgMode=y
        fi

        if [[ "$url" =~ '^(?:https?://)?[^/]*youtube.com(?:/embed/([^/]*))' ]] ; then
            local id="$match[1]"
            img="https://i.ytimg.com/vi/${id}/maxresdefault.jpg" # embedded videos don't set their bloody meta tags
        fi
    fi
    local mode="${2:-md}"

    # isLocal && local fhMode="${fhMode:-curlfast}" # servers are fast enough to work with the default fhMode

    local html="${html:-$(full-html2 "$url")}"
    local cleanedhtml="${cleanedhtml:-$(<<<"$html" readability "$url")}" # takes ~1.5s

    # old: # meta=( "${(@0)$(urlmeta $url all)}" ) # takes ~0.475s
    meta=( "${(@0)$(urlmeta2 $url title description image author)}" ) # takes ~0.04s
    title="${url2note_override_title:-$meta[1]}"
    title="$(ecn "$title" | prefixer -o ' ' --skip-empty | str2orgtitle)"
    desc="${meta[2]}"
    desc="$(<<<$desc html2utf.py)"
    desc="$(ecn "$desc" | prefixer -o ' ' --skip-empty)"
    img="${meta[3]:-$img}"
    author="$meta[4]"
    readest=""
    if [[ "$cleanedhtml" != no ]] ; then
        readest="$(<<<"$cleanedhtml" html-get-reading-estimate /dev/stdin)" @TRET # takes ~0.25s
    fi

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
            indent="** "
            ec "* [[$(ecn $url| url-encode.py)][${title:-$url}]]"
        else
            # we insert the links with the heading already created in emacs.
            indent=""
            ec "[[$(ec $url| url-encode.py)][${title:-$url}]]"
        fi
        test -n "$author" && ec "${indent}By: $author"
        test -n "$readest" && ec "${indent}$readest"
        test -n "$desc" && ec "${indent}$desc"
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
##
function urls-cleansharps1() {
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
    arrNN ${(@u)newUrls}
}

function urls-cleansharps() {
    local urls
    urls=(${(@fu)"$(command rg --replace '$1' '([^#]*)(#.*)?')"}) @TRET
    arrNN $urls[@]
}
noglobfn urls-cleansharps

aliasfn-ng urlc urls-cleansharps
aliasfn-ng url-clean-hash urls-cleansharps
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

function url-date() {
    local url="$1" date

    date="$(url-goometa "$url")" # Google's metadata can contain irrelevant stuff, but if they usually contain the date, and are more accurate than wayback's.
    if [[ "$date" =~ '^\s*$' ]] ; then
        date="$(url-date-wayback "$url")"
    fi
    ec $date
}
##
