# typeset -A sharif_deps=(civil 20 industrial 21 math 22 chemistry 23  physics 24 electrical 25 oil 26 material 27 mechanical 28 "" 29 "" 30 "" 31 "" 32 "" 33)
function sharif-dep-getIDs() {
    typeset -gA sharif_deps=()

    local data
    data="$(curl "$sharif_curl_opts[@]" 'https://edu.sharif.edu/action.do' \
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

    local out=''
    out="$(curl "$sharif_curl_opts[@]" 'https://edu.sharif.edu/action.do' \
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
        --compressed | pup 'body > table > tbody > tr:nth-child(2) > td > form')" || return $?
    if test -z "$out" ; then
        tnotif-casual "$0: Empty: $name ($depID)"
        return 1
    fi
    ec '<html dir="rtl"><head><link rel="stylesheet" type="text/css" href="https://edu.sharif.edu/css/default.css"></head><body>' > $dest
    ec "$out" >> $dest

    ec '</body></html>' >> $dest

    prettier --write "$dest"
    local destjson="${dest:r}.json" oldjson=''
    if test -e "$destjson" ; then
        oldjson="$(cat $destjson)"
    fi
    <$dest sharif-dep-2json > $destjson
    if test -n "$oldjson" ; then
        local diff="$(marked_courses='[9999,9998,44737]' sharif_diff.js =(ec "$oldjson") "$destjson")"
        if test -n "$diff" ; then
            if false ; then
                # EDU_DIFF channel
                # @idea also send to channel based on dep ID, to reduce the clutter
                tsend -- -1001472917717 "${dest:t:r}"$'\n'"$diff"
            fi

            color blue "$diff"
        fi
    fi
}
function sharif-dep-2json() {
    table2json2.js | jq '[.[]| .[] |
        if .[0] != "گروه" then
            {
            "Group": .[0],
            "Credits": .[1],
            "Name": .[2],
            "Prereqs": .[3],
            "Capacity": .[4],
            "EnrolledStudents": .[5],
            "Professor": .[6],
            "ExamDate": .[7],
            "ClassTime": .[8],
            "Comments": .[9],
            "MessageOnSignup": .[10],
            "CourseID": .[11]
            }
        else
            empty
        end
        ]'
}
function sharif-dep-save-all() {
    sharif-dep-getIDs || return $?
    local ret=0
    for name id in "${(@kv)sharif_deps}" ; do
        reval-ecdate sharif-dep-save "$id" "$name" || {
            ecerr "Failed with $? for $name ($id)"
            ret=1
        }
    done
    aa --allow-overwrite=true https://edu.sharif.edu/css/default.css # to backup the CSS as well.
    return $ret
}

function sharif-dep-git-update {
    assert sharif-login @RET

    pushf "$sharif_dir" || return $?
    {
        { sharif-dep-save-all | rtl-reshaper-streaming } @TRET
        git add --all @TRET
        if test -n "$(git status --porcelain)" ; then
            local timestamp
            timestamp="$(jalalicli today -j 'yyyy/MM/dd HH:mm:ss')" @TRET
            dirindex_gen.py --skipmod --filter '*.html' @TRET
            git commit -a -m "Updated at: $timestamp" @TRET
        fi
        git push --set-upstream origin master @TRET
    } always { popf }
}

function sharif-dep-update-continuous {
    local sleep="${1:-600}"

    local m s
    pushf "$sharif_dir" || return $?
    {
        while true ; do
            sharif-login
            fnswap prettier true sharif-dep-save-all # no need to prettify the HTMLs
            ecdate "$0: Updated"
            s="$sleep"
            m="$(gdate '+%M')" # minutes since last hour
            m=$(( 60 - m ))
            if (( m < 10 )) ; then
                s=$(( m * 60 ))
            elif (( m > 50 )) ; then
                s=30
            fi
            reval-ecdate sleep-neon "$s"
        done
    } always { popf }
}
##
sharif_tmp_dir=~/tmp/shariflogin
sharif_dir="$codedir/data/sharif_course_list"
sharif_cjar="$sharif_tmp_dir/cjar.txt"
sharif_curl_opts=(--silent --fail --no-progress-meter --cookie $sharif_cjar --cookie-jar $sharif_cjar --max-time 10 --retry 20 --retry-delay 1)

function sharif-login {
    # pushf ~/tmp/"$(uuidm)"
    pushf $sharif_tmp_dir || return $?
    {
        silent command rm l.html l2.html "$sharif_cjar" jc.jpg
        local cjar jcaptcha
        cjar="$sharif_cjar"
        curl "$sharif_curl_opts[@]" 'https://edu.sharif.edu/login.do' > l.html

        # curl -v --fail --no-progress-meter --cookie $cjar --cookie-jar $cjar 'https://edu.sharif.edu/' \
            #     -H 'authority: edu.sharif.edu' \
            #     -H 'pragma: no-cache' \
            #     -H 'cache-control: no-cache' \
            #     -H 'dnt: 1' \
            #     -H 'upgrade-insecure-requests: 1' \
            #     -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36' \
            #     -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' \
            #     -H 'sec-fetch-site: none' \
            #     -H 'sec-fetch-mode: navigate' \
            #     -H 'sec-fetch-user: ?1' \
            #     -H 'sec-fetch-dest: document' \
            #     -H 'accept-language: en-US,en;q=0.9,fa;q=0.8,ru;q=0.7,ur;q=0.6' \
            #     --compressed > l.html

        ##
        # cfTimeout=5 curlfull.js 'https://edu.sharif.edu/' cjs.json > l.html
        # local cookie="$(<cjs.json jqm '.[] | .name + "=" + .value')"
        ##

        jcaptcha="https://edu.sharif.edu/$(<l.html command rg --only-matching -e 'src="(jcaptcha\.jpg\?rand=[^"]*)"' --replace '$1')"

        local retries=0
        while true ; do
            retries=$((retries+1))

            curl "$sharif_curl_opts[@]" "$jcaptcha" -o jc.jpg # downloading the image resets the captcha

            # @opts height 50 @ icat-go jc.jpg
            icat jc.jpg

            local solved_captcha="$(tesseract jc.jpg stdout -c tessedit_char_whitelist=0123456789 |trimsed)"

            typ solved_captcha
            # vared solved_captcha
            # typ solved_captcha

            curl "$sharif_curl_opts[@]" 'https://edu.sharif.edu/login.do' \
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
                -H 'referer: https://edu.sharif.edu/login.do' \
                -H 'accept-language: en-US,en;q=0.9,fa;q=0.8,ru;q=0.7,ur;q=0.6' \
                --data-raw 'username='$sharif_username'&password='$sharif_password'&jcaptcha='$solved_captcha'&x=0&y=0&command=login&captcha_key_name=null&captchaStatus=false' \
                --compressed > l2.html

            # open l2.html
            # sleep 5

            if < l2.html command rg --quiet 'کاربر جاری' ; then
                ec login successful
                break
            else
                if ask 'retry?' ; then
                        continue
                else
                    return 1
                fi

                # if (( retries <= 20 )) ; then
                #     continue
                # else
                #     retries=0
                #     reval-ecdate sleep-neon 900
                #     continue
                # fi
            fi
        done
    } always {
        popf
    }
}
##
function sharif-register-course {
    local dest=~/tmp/reg_ans.html
    local course_id="${1}"
    assert-args course_id @RET

    ecbold "course_id: ${course_id}"

    reval-ec sharif-login @RET

    reval-ec sharif-goto-register @RET

    curl "$sharif_curl_opts[@]" \
        'https://edu.sharif.edu/register.do?command=add' \
        -H 'authority: edu.sharif.edu' \
        -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' \
        -H 'accept-language: en-US,en;q=0.9,fa;q=0.8' \
        -H 'cache-control: no-cache' \
        -H 'content-type: application/x-www-form-urlencoded' \
        -H 'dnt: 1' \
        -H 'origin: https://edu.sharif.edu' \
        -H 'pragma: no-cache' \
        -H 'referer: https://edu.sharif.edu/register.do?command=add' \
        -H 'sec-ch-ua: "Chromium";v="104", " Not A;Brand";v="99", "Google Chrome";v="104"' \
        -H 'sec-ch-ua-mobile: ?0' \
        -H 'sec-ch-ua-platform: "macOS"' \
        -H 'sec-fetch-dest: document' \
        -H 'sec-fetch-mode: navigate' \
        -H 'sec-fetch-site: same-origin' \
        -H 'sec-fetch-user: ?1' \
        -H 'upgrade-insecure-requests: 1' \
        -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/104.0.0.0 Safari/537.36' \
        --data-raw "lessonID=${course_id}&groupID=&unit=" \
        --compressed > "$dest" @STRUE

    open "$dest"
}

function sharif-goto-register {
    curl "$sharif_curl_opts[@]" 'https://edu.sharif.edu/action.do' \
        -H 'authority: edu.sharif.edu' \
        -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' \
        -H 'accept-language: en-US,en;q=0.9,fa;q=0.8' \
        -H 'cache-control: no-cache' \
        -H 'content-type: application/x-www-form-urlencoded' \
        -H 'dnt: 1' \
        -H 'origin: https://edu.sharif.edu' \
        -H 'pragma: no-cache' \
        -H 'referer: https://edu.sharif.edu/action.do' \
        -H 'sec-ch-ua: "Chromium";v="104", " Not A;Brand";v="99", "Google Chrome";v="104"' \
        -H 'sec-ch-ua-mobile: ?0' \
        -H 'sec-ch-ua-platform: "macOS"' \
        -H 'sec-fetch-dest: document' \
        -H 'sec-fetch-mode: navigate' \
        -H 'sec-fetch-site: same-origin' \
        -H 'sec-fetch-user: ?1' \
        -H 'upgrade-insecure-requests: 1' \
        -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/104.0.0.0 Safari/537.36' \
        --data-raw 'changeMenu=OnlineRegistration*registerJustStudent&isShowMenu=&id=&commandMessage=&defaultCss=' \
        --compressed
}
##
