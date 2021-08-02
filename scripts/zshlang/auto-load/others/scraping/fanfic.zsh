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
