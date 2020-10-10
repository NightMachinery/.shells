# typeset -A sharif_deps=(civil 20 industrial 21 math 22 chemistry 23  physics 24 electrical 25 oil 26 material 27 mechanical 28 "" 29 "" 30 "" 31 "" 32 "" 33)
function sharif-dep-getIDs() {
    typeset -gA sharif_deps=()

    local data
    data="$(curlm 'https://edu.sharif.edu/action.do' \
        -H 'authority: edu.sharif.edu' \
        -H 'pragma: no-cache' \
        -H 'cache-control: no-cache' \
        -H 'origin: https://edu.sharif.edu' \
        -H 'upgrade-insecure-requests: 1' \
        -H 'dnt: 1' \
        -H 'content-type: application/x-www-form-urlencoded' \
        -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36' \
        -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' \
        -H 'sec-fetch-site: same-origin' \
        -H 'sec-fetch-mode: navigate' \
        -H 'sec-fetch-user: ?1' \
        -H 'sec-fetch-dest: document' \
        -H 'referer: https://edu.sharif.edu/action.do' \
        -H 'accept-language: en-US,en;q=0.9,fa;q=0.8,ru;q=0.7,ur;q=0.6' \
        --data-raw 'changeMenu=OnlineRegistration*OfficalLessonListShow&isShowMenu=&commandMessage=&defaultCss=' \
        --compressed)" || return $?

    data="$(<<<"$data" pup 'select[name="depID"] option json{}')" || return $?
    local ids=("${(@f)$(<<<$data jqm '.[]|.value')}")
    local names=("${(@f)$(<<<$data jqm '.[]|.text')}")

    local id name i=0
    for id in "$ids[@]" ; do
        i=$((i+1))
        name=$names[$i]
        if [[ "$name" == '-' ]] ; then
            continue
        fi
        sharif_deps[$name]=$id
        ec "$name = $id"
    done
}
function sharif-dep-save() {
    local depID="${1:?}" # ID
    local name="${2:-$dep}"

    local dest="${name}.html"
    ec '<html dir="rtl"><head><link rel="stylesheet" type="text/css" href="https://edu.sharif.edu/css/default.css"></head><body>' > $dest

    curlm 'https://edu.sharif.edu/action.do' \
        -H 'authority: edu.sharif.edu' \
        -H 'pragma: no-cache' \
        -H 'cache-control: no-cache' \
        -H 'origin: https://edu.sharif.edu' \
        -H 'upgrade-insecure-requests: 1' \
        -H 'dnt: 1' \
        -H 'content-type: application/x-www-form-urlencoded' \
        -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36' \
        -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' \
        -H 'sec-fetch-site: same-origin' \
        -H 'sec-fetch-mode: navigate' \
        -H 'sec-fetch-user: ?1' \
        -H 'sec-fetch-dest: document' \
        -H 'referer: https://edu.sharif.edu/action.do' \
        -H 'accept-language: en-US,en;q=0.9,fa;q=0.8,ru;q=0.7,ur;q=0.6' \
        --data-raw 'level=0&teacher_name=&sort_item=1&depID='$depID \
        --compressed | pup 'body > table > tbody > tr:nth-child(2) > td > form' >> $dest || return $?

    ec '</body></html>' >> $dest

    prettier --write "$dest"
}
function sharif-dep-save-all() {
    sharif-dep-getIDs || return $?
    local ret=0
    for name id in "${(@kv)sharif_deps}" ; do
        reval-ec sharif-dep-save "$id" "$name" || {
            ecerr "Failed with $? for $name ($id)"
            ret=1
        }
    done
    aa https://edu.sharif.edu/css/default.css # to backup the CSS as well.
    return $ret
}
function sharif-dep-git-update() {
    pushf "$codedir/data/sharif_course_list"
    {
        sharif-dep-save-all
        git add --all
        if test -n "$(git status --porcelain)" ; then
            local timestamp="$(jalalicli today -j 'yyyy/MM/dd HH:mm:ss')"
            dirindex_gen.py --skipmod --filter '*.html'
            git commit -a -m "Updated at: $timestamp" || return $?
            # git push
            git push --set-upstream origin master
        fi
    } always { popf }
}
