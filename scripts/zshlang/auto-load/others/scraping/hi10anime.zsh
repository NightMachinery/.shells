##
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
##
function hi10-new-check() {
    local query="${1}"
    assert-args query @RET

    local new="$(full-html2 https://hi10anime.com/ | pup '.entry-title a text{}')"
    <<<$new rg --ignore-case "$query" # --quiet
}

function h_hi10-new-notify() {
    local query="${1:?}" # @regex with ignore-case

    local found
    if found="$(hi10-new-check "$query")" ;  then
        tnotifc "hi10: $found"
        # sleep $((3*86400))
        sleep 86400 # = 3600*24
    else
        sleep 86400 # = 3600*24
    fi
}

function hi10-new-notify() {
    loop h_hi10-new-notify "$@"
}
##
function hi10-cook() {
    local url="$1"
    local title="$(urlmeta $url title|str2tmuxname)"
    local cmd="FORCE_INTERACTIVE=y $(cook hi10-rc hi10-ng "$url")"
    ec-copy "tmux new -s $(gq "$title") zsh -c $(gq "$cmd")"
}
##
function hi10-ng {
    mdoc "$0 <url-of-hi10-page> [<regex>]
Use hi10-cook to copy the necessary command for pasting in a remote server." MAGIC
    local url="$1" dest="${hi10_ng_dest}"
    local regex=${2:-'\.mkv$'}

    pxa-maybe

    if test -z "$dest" ; then
        local title="$(fhMode=aacookies urlmeta $url title)"
        [[ "${$(pwd):t}" == "$title" ]] || cdm "$title"
    else
        cdm "$dest"
    fi

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

    bell-dl-ready

    arrN "${(@u)pArgs}" | fz | tee "hi10-links.txt" | hi10-dl # (u) makes the array elements unique.
}

function hi10-dl {
    #: `hi10-dl < hi10-links.txt`
    #     magic mdoc "$0 < hi10-links.txt
    # Generates jtokens for links and downloads them." ; mret
    ##

    inargsf mapg '$i$(hi10-jtoken)' | inargsf rgeval $proxyenv aa -j2 -Z

    bell-dl
}

function hi10-from-page() {
    mdoc "DEPRECATED. Use hi10-ng" MAGIC

    # You need to have persistent cookies in lynx, and have logged in.
    hi10-multilink "${(@f)$(lynx -cfg=~/.lynx.cfg -cache=0 -dump -listonly $1|grep -E -i ${2:-'.*\.mkv$'})}"
    # eval 'hi10-multilink ${(@f)$(lynx -cfg=~/.lynx.cfg -cache=0 -dump -listonly "'"$1"'"|grep -E -i "'"${2:-.*\.mkv$}"'")}'
}
##
