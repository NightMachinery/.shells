##
function cookies2json() {
    local c domains=( $cookies2json_d[@] )
    c="$(in-or-args "$@")" @TRET
    assert-args c domains @RET

    if [[ "$c" =~ 'Cookie:(.*)' ]] ; then
        c="$match[1]"
    fi

    {
        local key val first=y domain
        ec '['
        for domain in $domains[@] ; do
            for cookie in ${(@ps.;.)c} ; do
                if [[ "$cookie" =~ '([^=]+)=(.*)' ]] ; then
                    if test -n "$first" ; then
                        first=''
                    else
                        ec " , "
                    fi
                    ##
                    ec '{'

                    key="$match[1]"
                    val="$match[2]"

                    ec '"name" : '
                    jq-quote "$key"
                    ec ' , "value" : '
                    jq-quote "${val}"
                    ec ' , "domain" : '
                    jq-quote "$domain"

                    ec '}'
                fi
            done
        done
        ec ']'
    } | jq .

    ## tests:
    # `cookies 'https://www.goodreads.com' | @opts d [ '.goodreads.com' 'www.goodreads.com' ] @ cookies2json`
    ##
}
##
