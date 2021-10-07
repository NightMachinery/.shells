##
function reddit2old {
   @inargsf

   arrN "$@" | sdlit 'https://www.reddit.com' 'https://old.reddit.com'
}
##
function reddit-sub-age {
    local url="$1"
    assert-args url @RET

    url="$(reddit2old "$url")"

    full-html2 "$url" | pup '.age' | html2org /dev/stdin
}
##
function reddit2org {
    local permalink="$1" dest="${2}"
    assert-args permalink @RET
    if ! [[ "$permalink" == http* ]] ; then
        permalink="https://old.reddit.com${permalink}"
    else
        permalink="$(reddit2old "$permalink")" @TRET
    fi

    local html
    html="$(fhSecure="${fhSecure:-n}" full-html2 "$permalink")" @TRET

    dest="$(h-org-dest-from-url "$url" "$dest")" @TRET

    {
        ec $html | pup '.content .title, .content .usertext'
    } | html2org /dev/stdin "$dest" >&2 @TRET

    ec "$dest"
}

function org2epub-from-url {
    local url="$1" dest="${2}" engine=("${org2epub_from_url_e[@]}")
    assert-args url engine @RET

    local org_file
    org_file="$(reval "$engine[@]" "$url" "${dest:r}")" @TRET

    @opts url "$url" @ org2epub-auto-metadata "$org_file"
}

aliasfn reddit2epub org2epub_from_url_e=reddit2org org2epub-from-url
##
function reddit-sub-posts-urls-pushshift {
    ecerr "$0: deprecated; Use reddit_sub_posts_urls_pushshift.py instead."
    # return 1
    ##
    local sub="${reddit_sub_posts_urls_sub:-rational}" from="${reddit_sub_posts_urls_from:-1}" to="${reddit_sub_posts_urls_to:-10}"

    if [[ "$to" == '_' ]] ; then
        to="$from"
    fi

    for i in {${from}..${to}} ; do
        # @warn this still assumes there aren't more than a 100 posts in a month (=size= is limited to 100 max), which is a false assumption even for r/rational
        before="$(jalalicli today -j unix --inc-month=-$(( i - 1 )) )"
        after="$(jalalicli today -j unix --inc-month=-${i})"

        revaldbg gurl "https://api.pushshift.io/reddit/search/submission/?subreddit=${sub}&sort=desc&sort_type=created_utc&after=${after}&before=${before}&size=101" | \
            jqm '.data[] | .full_link'
    done
    ##

    ##
}
function reddit-sub-posts-urls {
    ecerr "$0: @warn reddit's API does not return more than the most recent 1000 posts"
    ##
    local url last_id d
    while true ; do
        url="https://www.reddit.com/r/rational.json?count=1000"
        if test -n "$last_id" ; then
            url+="&after=${last_id}"
        fi

        d="$(gurl "$url")" @TRET

        ec "$d" | jqm '.data.children[] | .data.permalink' | prefixer --skip-empty --add-prefix 'https://www.old.reddit.com'
        ec

        last_id="$(ec "$d" | jqm '.data.after')" || last_id=''
        if test -z "$last_id" ; then
            ecbold "$0: Finished"
            return 0
        fi
        ecbold '##################'
        re typ url last_id >&2
        ecbold '##################'

        # sleep 0.1
    done
}
##
