##
function fanficfare-m {
    $proxyenv fanficfare --non-interactive -f mobi "$@"
}

function jfic {
    jee

    local i
    for i in "$@" ; do
        silent aa -- "$(urlmeta "$i" image)"
    done
    re "fanficfare --non-interactive" "$@"
    sout re p2k *.epub(ND)
}
##
function fanficfare2org {
    local url="$1"
    assert-args url @RET

    local d
    # fanficfare still creates a junk epub file with --meta-only
    d="$(indir "$(gmktemp -d)" fanficfare --meta-only --json-meta "$url")" @TRET

    ec "$d" | fanficfare2org.lisp @TRET
}

##
function ficwad {
    # https://ficwad.com/
    ##

    local url="$1"
    assert-args url @RET

    ecgray "$0: @warn x-rated fics require cookies to download"

    local urls
    urls=("${(@f)$(getlinks-c "$url" | ugbool '/story/\d+$ -/reviews')}") @TRET

    fnswap urlfinalg arrN tlrl-ng -o . "$urls[@]"
    ## tests
    # url='https://ficwad.com/story/227774'
    # fnswap urlfinalg arrN tlrl-ng "$urls[@]"
    ##
}
##
function fichub {
    local url="$1"

    $proxyenv reval-ec fichub_cli --format mobi --url "$url"
}

function jfichub {
    jee

    local urls=($@)

    local url
    for url in "$urls[@]" ; do
        if ! $proxyenv revaldbg fichub_cli --format epub -o . --url "$url" ; then
            ecerr "$0: URL $(gquote-sq "$url") failed. Continuing."
        fi
    done

    dir2k .
}
##
