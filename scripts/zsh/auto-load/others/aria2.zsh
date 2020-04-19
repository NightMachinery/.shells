aa-raw() {
    local opts=()
    isI || opts+=(--show-console-readout false --summary-interval 0)
    aria2c --seed-time=0 --max-tries=0 --retry-wait=1 $opts[@] "$@" #-Z has some unsavory sideeffects so I have not included it in this.
}
aagh() { aa "${(@f)$(gh-to-raw "$@")}" }
getcookies() {
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
    mdoc "$0 <cookies-or-url> <aa-args>
If cookie is to be extracted by a URL, that URL will also be downloaded." MAGIC

    local url="$1"
    local c="Cookie:$1"
    if [[ "$url" =~ '^http' ]]
    then
           c="Cookie:$(getcookies "$url")"
    else
        shift
    fi
    aa --header=$c $@
    # aria2c --load-cookies =(getcookies $url) $url
}
