##
function libgendl-md5-main {
    local md5="$1"

    # local ipfs_mirror="${libgen_ipfs:-all}"
    local ipfs_mirror="${libgen_ipfs:-main}"
    # local ipfs_mirror="${libgen_ipfs:-pinata}"

    # local mainmirror="http://93.174.95.29"
    # local mainmirror="http://31.42.184.140"
    # local mainmirror="http://62.182.86.140"
    # local mainmirror="http://193.218.118.42"
    local mainmirror="http://176.119.25.72"
    # local mainmirror="http://library.lol"

    # local url="http://gen.lib.rus.ec/get?md5=${md5}&open=0"
    local urls=( "$mainmirror/main/${md5}" "$mainmirror/fiction/${md5}" )

    local url_patterns_include=()
    if [[
           # "$ipfs_mirror" == "all" ||
           #: Cloudflare seems to be always down nowadays ...
               "$ipfs_mirror" == "cloudflare" || "$ipfs_mirror" == 'cf' ]] ; then
        url_patterns_include+='cloudflare-ipfs.com/'
    fi
    if [[ "$ipfs_mirror" == "all" || "$ipfs_mirror" =~ '^ipfs(\.io)?$' ]] ; then
        url_patterns_include+='ipfs.io/'
    fi
    if [[ "$ipfs_mirror" == "all" || "$ipfs_mirror" =~ '^infura(\.io)?$' ]] ; then
        url_patterns_include+='infura.io/'
    fi
    if [[ "$ipfs_mirror" == "all" || "$ipfs_mirror" =~ '^pinata$' ]] ; then
        url_patterns_include+='pinata.cloud/'
    fi
    if [[ "$ipfs_mirror" == "all" || "$ipfs_mirror" =~ '^main$' ]] ; then
        url_patterns_include+="$mainmirror"
        url_patterns_include+=(
            http://download.library
            https://download.library
        )
    fi

    local rg_opts=() i
    for i in ${url_patterns_include[@]} ; do
        rg_opts+=(-e "$i") #: OR clauses
    done

    #: '\.[^/]+$'
    revaldbg getlinks-c -e '\.(?:'"${(j.|.)ebook_formats}"')$' "$urls[@]" | {
        if isDbg ; then
            tee /dev/stderr
        else
            cat
        fi
        } | {
        # The main mirror (not the IPFS ones) might get false positives if the name of the book contains a dot. We can whitelist allowed formats but that is too costly ...
        revaldbg rg -F "${rg_opts[@]}"
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
            aa_split="${aa_split:-4}" dl-multi $links[@] @RET
            #: Setting the split number too high can make the IPFS servers ban us. I don't know if setting them low can solve this issue though.
        else
            ecerr "$0: No books found for md5: $md5"
            return 1
        fi
    }
}
reify libgendl-md5-main libgendl-md5-bok libgendl-md5-old libgendl-md5-bok-old libgendl-md5

function jlibplain {
    # libgendl-md5-main "${(f@)$(re libgen2md5 "$@")}" | inargsf aa -Z
    revaldbg libgendl-md5 "${(f@)$(libgen2md5 "$@")}"
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
