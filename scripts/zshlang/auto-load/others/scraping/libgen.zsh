##
function libgendl-md5-main {
    local md5="$1"
    local ipfs_mirror="${libgen_ipfs:-main}"

    # local mainmirror="http://93.174.95.29"
    # local mainmirror="http://31.42.184.140"
    local mainmirror="http://62.182.86.140"

    # local url="http://gen.lib.rus.ec/get?md5=${md5}&open=0"
    local urls=( "$mainmirror/main/${md5}" "$mainmirror/fiction/${md5}" )

    if [[ "$ipfs_mirror" == "cloudflare" || "$ipfs_mirror" == 'cf' ]] ; then
        ipfs_mirror='cloudflare-ipfs.com/'
    elif [[ "$ipfs_mirror" =~ '^ipfs(\.io)?$' ]] ; then
        ipfs_mirror='ipfs.io/'
    elif [[ "$ipfs_mirror" =~ '^infura(\.io)?$' ]] ; then
        ipfs_mirror='infura.io/'
    elif [[ "$ipfs_mirror" =~ '^pinata$' ]] ; then
        ipfs_mirror='pinata.cloud/'
    elif [[ "$ipfs_mirror" =~ '^main$' ]] ; then
        ipfs_mirror="$mainmirror"
    fi

    getlinks-c -e '\.[^/]+$' "$urls[@]" | {
        # The main mirror (not the IPFS ones) might get false positives if the name of the book contains a dot. We can whitelist allowed formats but that is too costly ...
        rg -F "$ipfs_mirror"
    }
}

function libgendl-md5-bok {
    local outs="$(libgendl-md5-bok-helper "$1" | inargsf bok.py)"
    test -e "$outs" # we expect only a single download
}

function libgendl-md5-bok-helper {
    local md5="$1"
    local url="https://b-ok.cc/md5/$md5"
    getlinks-c -e '/book/' "$url" |gsort -u
}

function libgendl-md5-bok-old {
    (( ${+commands[bok.js]} )) || { ecerr 'bok.js not found.' ; return 1 }
    libgendl-md5-bok-helper "$1" |inargsf re "gtimeout 15m bok.js"
}

function libgendl-md5-old {
    local bok="$(libgendl-md5-bok-old $1)"
    if test -n "$bok" ; then
        aa "$bok"
    else
        libgendl-md5-main "$1" |inargsf aa -Z
    fi
}

function libgendl-md5 {
    local md5="$1"
    local lgNoBok="${lgNoBok:-y}" # bok is useless now

    { { ! bool "$lgNoBok" } && libgendl-md5-bok "$md5" } || {
        { ! bool "$lgNoBok" } && ecerr "bok failed. Trying main ..."

        local links=( ${(@f)"$(libgendl-md5-main "$md5")"} )
        if (( ${#links} >= 1 )) ; then
            dl-multi $links[@] @RET
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
@opts-setprefix jlibplain libgen

function jlib() {
    jee
    jlibplain "$@" || return 1
    dir2k
    true
}
@opts-setprefix jlib libgen

function libgen2md5() {
    [[ "$1" =~ '(\w{32})\W*$' ]] && print -r -- "$match[1]"
}
reify libgen2md5
noglobfn libgen2md5
##
