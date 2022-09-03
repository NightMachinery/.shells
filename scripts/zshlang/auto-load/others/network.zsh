###
function curl-ip-nodns {
    assert-args lilf_ip @RET

    env-clean curl --resolve lilf.ir:80:"$lilf_ip" --resolve lilf.ir:443:"$lilf_ip" https://lilf.ir/
}

function curl-ip {
    local opts
    opts=( --progress-bar --retry 120 --retry-delay 1 "$@" )

    local res
    res="$(reval-ec curl "$opts[@]" https://ipinfo.io)" @TRET

    local jq_opts=()
    if isColorTty ; then
        jq_opts+='--color-output'
    fi

    local res_json
    if res_json="$(ec $res | serr jq -e "$jq_opts[@]" .)" ; then
        ec $res_json
    else # ipinfo blocks Iranian IPs
        local ip
        ip="$(reval-ec curl "$opts[@]" https://checkip.amazonaws.com)" @RET
        ec "$ip"
        ip-geolocate "$ip" @RET
        ##
        # ec $res | html2text
    fi
}
alias ci='curl-ip'
# socks5h resolves hostname through proxy (I think). It's faster for my youtube test reqs:
# curl -x socks5h://127.0.0.1:1081 -o /dev/null -w %{url_effective}  'https://www.youtube.com/'
# curl -x http://127.0.0.1:1087 -o /dev/null -w %{url_effective}  'https://www.ipinfo.io/'
# curl -x socks5h://172.17.0.1:1081 -o /dev/null -w %{url_effective}  'https://www.ipinfo.io'
# curl -x http://172.17.0.1:1087 -o /dev/null -w %{url_effective}  'https://www.ipinfo.io/'
# time2 brishzr curl -o /dev/null -w %{url_effective}  'https://www.youtube.com/watch?v=5X5v7vRYQjc&list=PL-uRhZ_p-BM7dYrgeHz4r3u74L9xwyXmL'
# time2 curl -x socks5h://127.0.0.1:1078 -o /dev/null -w %{url_effective}  'https://www.ipinfo.io/'

aliasfn ci78 curl-ip -x 'socks5h://127.0.0.1:1078'
aliasfn ci79 curl-ip -x 'socks5h://127.0.0.1:1079'
aliasfn ci80 curl-ip -x 'socks5h://127.0.0.1:1080'
aliasfn ci81 curl-ip -x 'socks5h://127.0.0.1:1081'
aliasfn ci90 curl-ip -x 'socks5h://127.0.0.1:1090'
aliasfn ci91 curl-ip -x 'socks5h://127.0.0.1:1091'

aliasfn ci87 curl-ip -x 'http://127.0.0.1:1087'
aliasfn ci88 curl-ip -x 'http://127.0.0.1:1088'
##
alias myip-amazon='curlm https://checkip.amazonaws.com'
alias myip-ipinfo='curlm https://ipinfo.io/ip'
##
# opendns sometimes returns wrong results, but it is slightly faster
alias myip-opendns='dig +short myip.opendns.com @208.67.222.222' # @resolver1.opendns.com
alias myip4-opendns='dig @resolver4.opendns.com myip.opendns.com +short -4'
alias myip6-opendns='dig @resolver1.ipv6-sandbox.opendns.com AAAA myip.opendns.com +short -6'

function myip-dnstoys {
    dig '+short' ip @dns.toys | gtr -d '"'
}
##
function myip-google1 {
    dig TXT +short o-o.myaddr.l.google.com @ns1.google.com | gtr -d '"'
}
function myip-google() {
    # `hyperfine 'dig TXT +short o-o.myaddr.l.google.com @8.8.8.8' 'dig TXT +short o-o.myaddr.l.google.com @8.8.4.4'`
    dig TXT +short o-o.myaddr.l.google.com @8.8.4.4 | rg --only-matching --replace '$1' '"edns0-client-subnet (.*)/\d+"'
}
##
function myip-cloudflare() {
    dig @1.1.1.1 ch txt whoami.cloudflare +short | gtr -d '"'
}
function myip-stun() {
    # servers: http://olegh.ftp.sh/public-stun.txt https://gist.github.com/mondain/b0ec1cf5f60ae726202e
    {
        # stunclient stun.stunprotocol.org
        stunclient 64.233.163.127 19302 # stun1.l.google.com 972.6 ms ± 218.9 ms
    } | rg --only-matching --replace='$1' 'Mapped address:\s+([^:]*):'
}

function myip-linux() {
    # @serveronly ? https://apple.stackexchange.com/questions/93533/how-to-obtain-the-external-ipv4-address-via-terminal
    hostname -I | awk '{print $1}'
}

function myip() {
    # these are all slow in Iran. If we could find some bloody Iranian service ...
    # https://iranwebmaster.net/t/ip/260
    
    if isServer ; then
        myip-linux
    else
        myip-google
    fi
}
##
# @fast Akamai approximate; not reliable for isIran.
# NOTE: This returns only an approximate IP from your block,
# but has the benefit of working with private DNS proxies.
function myipa-akami() {
    dig +short TXT whoami.ds.akahelp.net | rg --only-matching --replace '$1' '"(\d+[^"]+)"'
}
##
# ❯ hyperfine 'dig TXT +short o-o.myaddr.l.google.com @216.239.34.10' 'dig TXT +short o-o.myaddr.l.google.com @8.8.8.8' 'dig TXT +short o-o.myaddr.l.google.com @ns2.google.com'
# Benchmark #1: dig TXT +short o-o.myaddr.l.google.com @216.239.34.10
#   Time (mean ± σ):      1.019 s ±  0.660 s    [User: 2.5 ms, System: 5.3 ms]
#   Range (min … max):    0.365 s …  2.448 s    10 runs

# Benchmark #2: dig TXT +short o-o.myaddr.l.google.com @8.8.8.8
#   Time (mean ± σ):     791.0 ms ± 567.4 ms    [User: 2.6 ms, System: 5.5 ms]
#   Range (min … max):   236.1 ms … 1653.2 ms    10 runs

# Benchmark #3: dig TXT +short o-o.myaddr.l.google.com @ns2.google.com
#   Time (mean ± σ):     950.6 ms ± 475.9 ms    [User: 3.3 ms, System: 8.3 ms]
#   Range (min … max):   435.6 ms … 1538.6 ms    10 runs
##
# hyperfine --warmup 5 'curl https://checkip.amazonaws.com' 'curl https://ipinfo.io/ip' 'dig +short myip.opendns.com @resolver1.opendns.com' 'dig TXT +short o-o.myaddr.l.google.com @ns1.google.com'
# Benchmark #1: curl http://checkip.amazonaws.com
#   Time (mean ± σ):      1.934 s ±  0.245 s    [User: 9.1 ms, System: 10.7 ms]
#   Range (min … max):    1.576 s …  2.334 s    10 runs

# Benchmark #2: curl https://ipinfo.io/ip
#   Time (mean ± σ):      1.947 s ±  0.401 s    [User: 27.6 ms, System: 13.6 ms]
#   Range (min … max):    1.235 s …  2.731 s    10 runs

# Benchmark #3: dig +short myip.opendns.com @resolver1.opendns.com
#   Time (mean ± σ):     949.8 ms ± 491.8 ms    [User: 3.0 ms, System: 8.3 ms]
#   Range (min … max):   391.7 ms … 1564.9 ms    10 runs

# Benchmark #4: dig TXT +short o-o.myaddr.l.google.com @ns1.google.com
#   Time (mean ± σ):      1.473 s ±  1.686 s    [User: 2.8 ms, System: 7.8 ms]
#   Range (min … max):    0.509 s …  6.208 s    10 runs
##
# hyperfine 'dig @1.1.1.1 ch txt whoami.cloudflare +short' 'dig TXT +short o-o.myaddr.l.google.com @8.8.4.4'

# Benchmark #1: dig @1.1.1.1 ch txt whoami.cloudflare +short
#   Time (mean ± σ):     771.0 ms ± 484.4 ms    [User: 2.6 ms, System: 5.6 ms]
#   Range (min … max):   319.2 ms … 1589.8 ms    10 runs

# Benchmark #2: dig TXT +short o-o.myaddr.l.google.com @8.8.4.4
#   Time (mean ± σ):     681.0 ms ± 389.4 ms    [User: 2.6 ms, System: 5.9 ms]
# Range (min … max):   236.3 ms … 1212.3 ms    11 runs
##
# ❯ hyperfine 'dig TXT +short o-o.myaddr.l.google.com @8.8.4.4' 'stunclient stun.stunprotocol.org'
# Benchmark #1: dig TXT +short o-o.myaddr.l.google.com @8.8.4.4
#   Time (mean ± σ):      1.209 s ±  0.785 s    [User: 2.5 ms, System: 6.8 ms]
#   Range (min … max):    0.211 s …  2.747 s    10 runs

# Benchmark #2: stunclient stun.stunprotocol.org
#   Time (mean ± σ):      1.670 s ±  0.644 s    [User: 2.0 ms, System: 3.4 ms]
# Range (min … max):    0.845 s …  2.585 s    10 runs
###
function ncp {
    cat | gnc -c localhost 2000
}
##
function ip-internal-get {
    : "Use 'ipconfig getifaddr en1' for wireless, or 'ipconfig getifaddr en0' for ethernet."

    ifconfig | grep "inet "
}

function ip-internal-get1 {
    ip-internal-get | gsed 1d | rget 'inet\s+(\d+\.\d+\.\d+\.\d+)'
}
##
function http-static-py {
    python -m http.server "${1:-8000}"
}
##
function http-static-caddy {
    caddy file-server -browse -listen "${2:-0.0.0.0}:${1:-8000}"
}
aliasfn http-static http-static-caddy
##
aliasfn wifi-info wifi network # @darwinonly
##
aliasfn speedtest-py pipx run speedtest-cli
function speedtest-i() {
    local q="$(fz-createquery "$@")"

    speedtest-py --server "$(speedtest-py --list | fzp "$q" | ghead -n 1 | cut -d ')' -f1 | trimsed)"
}
##
typeset -g maxmind_db_1="$nightNotes/private/resources/databases/ip_geolocation_maxmind/GeoLite2-City.mmdb"

function ip-geolocate-mm-country() {
    ip-geolocate-mm "$@" | jqm '.[].Records[].Record | .country.names.en'
}

function ip-geolocate-mm() {
    local ip="${1:?}"

    local db="${maxmind_db_1}"
    mmdbinspect -db "$db" "$ip"
}

function ip-geolocate-mm1() {
    # https://unix.stackexchange.com/questions/7399/ip-to-country-console-command
    # https://dev.maxmind.com/geoip/geoipupdate/

    # use `man mmdblookup` for the lookup args
    local ip="${1:?}" lookup=("${@[2,-1]}")

    local db="${maxmind_db_1}"
    revaldbg mmdblookup --file $db --ip "$ip" $lookup[@]
}

function ip-geolocate2() {
    local ip="${1:?}"

    gurl ipinfo.io/"$ip"/geo
    # ipinfo has a rate limit. ifconfig.me provides a similar service (plus a REST-like API). Also ifonfo.io has blocked Iran.
}

function ip-geolocate1() {
    local ip="${1:?}"

    geo.bash -a "$ip" -o country
}

function ip-geolocate() {
    if test -e "${maxmind_db_1}" ; then
        ip-geolocate-mm-country "$@"
    else
        ip-geolocate1 "$@"
    fi
}
##
function mycountry() {
    local ip
    ip="$(myip)" || {
        ecerr "$0: No net"
        return 1
    }
    ip-geolocate "$ip" # ~0.9s
}
# alias mycountry='geo.bash -o country' # ~2.4s
# `whois "$(myip)"` has lots of info but is even more slow
##
function isIran() {
    # @alt isIranTLS
    # https://superuser.com/questions/1629020/whats-a-quick-way-to-check-if-a-tls-handshake-is-possible-with-twitter-com

    [[ "$(mycountry)" == Iran ]]

    ## faster
    # ! ping -q -c 1 -W 1 facebook.com &>/dev/null #  faster when it succeeds
    # ! ping -q -c 1 -W 1 69.171.250.35 &>/dev/null # (facebook's ip) succeeds even in Iran
}

function isIranTLS() {
    # takes 1.3s even if it succeeds, as openssl is blocking
    ! { { gtimeout 1.3s openssl s_client -connect www.youtube.com:443 ; true }  | rg -F 'BEGIN CERTIFICATE' }
}

function isIranHTTP() {
    # this works with any blocked HTTP website
    curl -s http://whatismyip.akamai.com/ | silent rg -F 'peyvandha.ir'
}
##
function bwh {
    bella_zsh_disable1

    sudo bandwhich -p -a "$@"
}
##
function darwin-net-static-set {
    networksetup -setmanual Wi-Fi 192.168.1.56 255.255.0.0 192.168.1.1
    ##
    # This did not work. We could not reach the internet and the LAN could not reach us.
    # networksetup -setmanualwithdhcprouter Wi-Fi 192.168.1.56
}

function darwin-net-static-unset {
    networksetup -setdhcp Wi-Fi
}
##
function net-local-wifi-enable-darwin {
    reval-ec networksetup -setnetworkserviceenabled FakeNet on
}

function net-local-wifi-disable-darwin {
    reval-ec networksetup -setnetworkserviceenabled FakeNet off
}
##
