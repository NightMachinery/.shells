aa-raw() {
    local opts=('--stderr=true')
    # Redirect all console output that would be otherwise printed in stdout to stderr.  Default: false

    isI || opts+=(--show-console-readout false --summary-interval 0)
    aria2c --seed-time=0 --max-tries=0 --retry-wait=1 $opts[@] "$@" #-Z has some unsavory sideeffects so I have not included it in this.
}
aagh() { aa "${(@f)$(gh-to-raw "$@")}" }
getcookies() {
    mdoc "$0 <url>
Will output in key=val;key2=val2
See |cookies| for higher-level API." MAGIC
    local url="$1"
    url="$url" python -c '
from pycookiecheat import chrome_cookies
import os
url = os.environ["url"]
HOME = os.environ["HOME"]
cookies = chrome_cookies(url, cookie_file=f"{HOME}/Library/Application Support/Google/Chrome/Default/Cookies")
#print(cookies)
out = ""
for k,v in cookies.items():
    out += f"{k}={v};"
print(out[:-1])
'

}
aacookies() {
    mdoc "$0 <aa-args>
Uses |theCookies| var or else feeds first URL to |cookies|." MAGIC

    if test -z "$theCookies"
    then
        local i
        for i in $@
        do
                 if [[ "$i" =~ '^http' ]]
                 then
                     c="$(cookies "$i")"
                     break 
                 fi
        done
    else
        c="$(cookies)"
    fi
    aa --header=$c $@
    # aria2c --load-cookies =(getcookies $url) $url
}
cookies() {
    mdoc "$0 [<cookie-or-url>=theCookies]
Outputs in header style." MAGIC

    local url="$1"
    local env_c="$theCookies"
    local c="Cookie:$1"

    if [[ "$url" =~ '^http' ]]
    then
           c="Cookie:$(getcookies "$url")"
    fi
    test -z "$url" && c="Cookie:${env_c}"
    ec "$c"
}
