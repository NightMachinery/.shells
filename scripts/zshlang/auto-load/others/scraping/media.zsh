##
function aamedia() {
    mdoc "$0 <page-url, 0 <= level of recursion <= 1 > ...
Scrapes media and audio links from the given pages, and then downloads them. Uses cookies (might need more config)." MAGIC

    local urls=( ${(u@)@} )

    local formats=( ${media_formats[@]} pdf )
    local regex='\.('"${(j.|.)formats}"')(\?[^/]*)?$'
    local url size matched
    for url in ${urls[@]}
    do
        url="$(url-final2 $url)" # url-final2 might be better as our URLs can be big files
        matched=''
        if [[ "$url" =~ "$regex" ]] ; then
            ec $url
            matched=y
            # even a URL that ends in, e.g., '.mkv' can be actually an HTML page that links to the actual file
        fi
        size="$(url-size "$url")" || {
            ecerr "$0: Could not get the size of URL '$url'"
            if test -z "$matched" ; then
                ecerr "$0: proceeding anyway ..."
                size=0
            else
                ecerr "$0: skipping it, as it has already matched"
                continue
            fi
        }
        if (( size < 5000000 )) ; then # 5 MB
            fnswap aria2c 'gtimeout 5m aria2c' getlinks-c -e $regex "$url"
        else
            ecerr "$0: Skipped big URL '$url'"
        fi
    done | gsort -u | { bello ; fzp } | {
        if isDbg
        then
            color 150 0 255 "$(cat)"
        else
            inargsf aacookies -Z
        fi
    }
}
noglobfn aamedia

function aamedia1() {
    : "aaCW2 <link-to-page-that-contains-media (level of recursion = 1; will support level 0 as well if its URL size is big)> ..."
    local theCookies fhMode=aacookies
    theCookies=${theCookies:-"$(cookies $1)"} || { ectrace ; bell-fail ; return $? }
    local urls=( ${(u@)@} )

    local formats=( ${media_formats[@]} pdf )
    local regex='\.('"${(j.|.)formats}"')$'

    typeset -ag titles=() links=() # @globalOutput

    local url t l
    for url in "${urls[@]}" ; do
        size="$(url-size "$url")"  || {
            ecerr "$0: Could not get the size of URL '$url'"
            ecerr "$0: proceeding anyway ..."
            size=0
        }
        if (( size > 2000000 )) ; then # 2 MB
            ecerr "$0: Link '$url' is too big, adding it as a file instead"
            # t="$(url-tail "$url")"
            t=''
            titles+="$t"
            links+="$url"
            continue
        fi
        t="$(ectrace_notrace=y url-title "$url")" || {
            if t="$(url-filename "$url")" && [[ "${t:l}" =~ "$regex" ]] ; then
                ecerr "$0: URL seems to be a file, proceeding with this assumption ..."
                titles+="${t:r}" # the ext is added from the URL anyway
                links+="$url"
            fi
            continue
        }
        l="$(getlinks-c "$url" -e "$regex" | gsort --unique)" || continue
        for l2 in ${(@f)l} ; do
            titles+="$t"
            links+="$l2"
        done
    done

    bell-dl-ready

    typeset -ag sel_i=() # @globalOutput
    for i in {1.."${#links}"} ; do
        l="${links[$i]}"
        t="${titles[$i]}"
        rtl_reshaper_wrap_p=n reval-rtl ecn "${t}:"$'\t'"$l"
        ec
    done | sponge | fz-masked "${(@F)links}" @RET # prints selected links to stdout
    # fz-masked needs to be the last process in the pipe or it'll fork and we will lose sel_i
    # do not rtl-reshape the links printed here, or they won't be copy-paste-able
    ec $'\n'"#######"$'\n'
    rgeval aamedia1-finish
}

function aamedia1-finish() {
    if test -e .aamedia1_links ; then
        silent trs .aamedia1_links.bak # it shoould be recoverable from the trash
        command mv .aamedia1_links .aamedia1_links.bak
    fi
    re typ links titles sel_i >&1 2>&2 &> .aamedia1_links # @globalInput
    # we can source this file to recover these variables
    ##
    local opts
    for i in ${sel_i[@]} ; do
        l="${links[$i]}"
        l="$(url-final2 "$l")" # beware not to use a downloading url-final function
        t="${titles[$i]}"
        test -z "$l" && continue
        reval-rtl ec "Downloading ${t}:"$'\n'"$l" #$'\n'
        opts=()
        if test -n "$t" ; then
            opts+=(-o "$(ec "${t}.${l:e}" | str2filename)")
            revaldbg aacookies "$opts[@]" "$l"
        else
            revaldbg ensure aa-remotename "$opts[@]" "$l" "$0"
            # revaldbg ensure curl-dl "$opts[@]" "$l" "$0"
        fi
    done

    bell-dl
    if isI && fn-isTop && fd-exists-d1 --ignore-case '\.webm$' &&ask "Run vid-fix?" Y ; then
        re 'assert vid-fix' *.webm @RET
        trs *.webm
    fi
}
##
