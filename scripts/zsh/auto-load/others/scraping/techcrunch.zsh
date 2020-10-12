function techcrunch-curl() {
    local url="${1:?}"
    local con="$(url-final $url)"
    if [[ "$con" =~ 'https://consent\.yahoo\.com/v2/collectConsent\?sessionId=(.*)' ]] ; then
        local sid="${match[1]}"
        dvar sid
        # sid='3_cc-session_36dd4d76-973e-407c-b328-8b546d2f6fc7'

        curl -o /dev/stdout --fail --location --cookie-jar =() 'https://consent.yahoo.com/v2/collectConsent?sessionId='$sid \
            -H 'Connection: keep-alive' \
            -H 'Pragma: no-cache' \
            -H 'Cache-Control: no-cache' \
            -H 'Origin: https://consent.yahoo.com' \
            -H 'Upgrade-Insecure-Requests: 1' \
            -H 'DNT: 1' \
            -H 'Content-Type: application/x-www-form-urlencoded' \
            -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36' \
            -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' \
            -H 'Sec-Fetch-Site: same-origin' \
            -H 'Sec-Fetch-Mode: navigate' \
            -H 'Sec-Fetch-User: ?1' \
            -H 'Sec-Fetch-Dest: document' \
            -H 'Referer: https://consent.yahoo.com/v2/collectConsent?sessionId='$sid \
            -H 'Accept-Language: en-US,en;q=0.9' \
            --data-raw 'sessionId='$sid'&originalDoneUrl='"$(<<<$url url-encode.py)"'&namespace=techcrunch&agree=agree&agree=agree' \
            --compressed
    else
        dvar con
        gurl $url
    fi
}
