##
function libgendl-md5-main() {
    local md5="$1"

    # local mainmirror="http://93.174.95.29"
    local mainmirror="http://31.42.184.140"

    # local url="http://gen.lib.rus.ec/get?md5=${md5}&open=0"
    local urls=( "$mainmirror/main/${md5}" "$mainmirror/fiction/${md5}" )

    getlinks-c -e '\.[^/]+$' "$urls[@]" | {
        # @outdatedComment will get false positives if the name of the book contains a dot. We can whitelist allowed formats but that is too costly ...
        rg -F 'cloudflare-ipfs.com'
    }
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
    local lgNoBok="${lgNoBok:-y}" # bok is useless now

    { test -z "$lgNoBok" && libgendl-md5-bok "$md5" } || {
        test -z "$lgNoBok" && ecerr "bok failed. Trying main ..."

        local links=( ${(@f)"$(libgendl-md5-main "$md5")"} )
        if (( ${#links} >= 1 )) ; then
            aa-multi $links[@] @RET
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
##
