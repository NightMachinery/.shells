##
function cookies-copy() {
    ec-copy "theCookies=$(gq "$(cookies-auto "$@")" "$@")"
}
aliasfn cook cookies-copy
function cookies-killlock() {
    if ask "$(fuser "$cookiesFile" | inargsf ps -fp)" ; then
        kill "$(serr fuser "$cookiesFile")"
    fi
}

function getcookies() {
    mdoc "[cookiesFile= ] $0 <url>
Will output Chrome's cookies for the given URL in key=val;key2=val2
See |cookies| for higher-level API." MAGIC
    local url="$1"
    local cf="${cookiesFile}"

    test -e "$cf" || { ecdbg "getcookies called with non-existent file: $cf" ; return 0 }

    local tmp
    if true ; then # @futureCron Did this solve the locking issue?
        tmp="$(gmktemp)"
        assert command cp "$cf" "$tmp" @RET
        cf="$tmp"
    fi

    {
        cookiesFile="$cf" url="$url" python -c '
from pycookiecheat import chrome_cookies
import os
url = os.environ["url"]
HOME = os.environ["HOME"]
cookiesFile = os.environ["cookiesFile"]
cookies = chrome_cookies(url, cookie_file=cookiesFile)
#print(cookies)
out = ""
for k,v in cookies.items():
    out += f"{k}={v};"
print(out[:-1])
'
        return $?
    } always {
        test -n "$tmp" && command rm -f "$tmp"
    }
}

function cookies() {
    mdoc "$0 [<cookie-or-url>=theCookies]
Outputs in header style." MAGIC

    local input="$1"
    local env_c="$theCookies"

    local c
    if [[ "$input" =~ '^http' ]]
    then
        c="Cookie:$(getcookies "$input")" @RET
    elif test -z "$input" ; then
        test -n "$env_c" && c="Cookie:${env_c}"
    else
        c="Cookie:$input"
    fi
    ec "$c"
}

function cookies-auto() {
    mdoc "Returns theCookies if present. Otherwise tries to get the cookies from the first url in args." MAGIC

    test -n "$caDisableCookies" && return 0

    local ci='' ret=0
    if test -z "$theCookies"
    then
        local i
        for i in $@
        do
            if [[ "$i" =~ '^http' ]]
            then
                c="$(serr cookies "$i")" && break || {
                        ret=1
                        break # the error is likely to be repeated anyway
                    }
            fi
        done
    else
        c="$(cookies)"
    fi

    ecn "$c"
    return $ret
}
##
function cookies2json() {
    local c domains=( ${cookies2json_d[@]:-NA} )
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

function cookies-extract() {
    local name="$1"
    assert-args name @RET

    cookies2json| jqm --arg n "$name" '.[] | select(.name | contains($n)) | .value'
}
##
