##
typeset -g proxycmd="${proxycmd:-command}"
typeset -g proxyenv="${proxyenv:-reval}"
###
# Do not add each IP in full - it will be too long and you won't be able to execute any commands in bash:
# printf -v no_proxy '%s,' 10.1.{1..255}.{1..255};
# export no_proxy="${no_proxy%,}";
##
export no_proxy='127.0.0.1,localhost'
###
function pxs-create() {
    typeset -g socksport="${1:-1081}"

    export pxs_env="ALL_PROXY=socks5h://127.0.0.1:$socksport"
    alias pxs="$pxs_env"
    alias pxs-local="local -x $pxs_env"
}
if isZii ; then
    pxs-create 1078
else
    pxs-create
fi

# alias pxs-maybe='isIran && pxs-local'
alias pxs-maybe='proxy-auto-p && pxs-local'
function reval-pxs() {
    reval pxs "$@"
}
##
proxy-env-unset () { #: proxy unexport
    #: @duplicateCode/aa8e6083b661b73d230497532ec267d6
    ##
    unset ALL_PROXY all_proxy http_proxy https_proxy HTTP_PROXY HTTPS_PROXY npm_config_proxy npm_config_https_proxy
}

alias noproxy='proxy_disabled=y'

function pxa-create {
    local px_httpport="${1:-1087}" name="${2:-pxa}"
    local px_http_ip="${3:-127.0.0.1}"
    # 1087: genrouter
    # 1088: shadowsocks
    # @todo0 make this support multiple ports at the  same time

    local v="${name}_env"
    export "$v"="ALL_PROXY=http://${px_http_ip}:${px_httpport} all_proxy=http://${px_http_ip}:${px_httpport} http_proxy=http://${px_http_ip}:${px_httpport} https_proxy=http://${px_http_ip}:${px_httpport} HTTP_PROXY=http://${px_http_ip}:${px_httpport} HTTPS_PROXY=http://${px_http_ip}:${px_httpport}"

    # silent re unalias pxa pxa-local
    #: @redundant

    alias "${name}"="${(P)v}"
    alias "${name}-local"="local -x ${(P)v}"
}
pxa-create
pxa-create 1087 pxa87
pxa-create 2091 pxa91
pxa-create 2092 pxa92
pxa-create 2041 pxa41
pxa-create 2032 pxa32
pxa-create 2035 pxa35
pxa-create 2089 pxa89
pxa-create 2096 pxa2096
pxa-create 2098 pxa98
pxa-create 2098 pxa2098
pxa-create 10809 pxateias 10.2.32.28

# alias pxa-maybe='isIran && pxa-local'
alias pxa-maybe='proxy-auto-p && pxa-local'

function reval-pxa {
    pxa reval "$@"
}

function proxy-active-p {
    test -n "${proxy_disabled}${ALL_PROXY}${all_proxy}${HTTP_PROXY}${http_proxy}${HTTPS_PROXY}${https_proxy}${FTP_PROXY}${ftp_proxy}"
}

function reval-pxa-if-no-proxy {
    #: The env var `no_proxy' is actually used by some Unix programs to exclude some IPs.

    if ! proxy-active-p ; then
        pxa reval-env "$@"
    else
        reval-env "$@"
    fi
}
##
function v2-on {
    tmuxnew v2ray-genrouter xray -config $nightNotes/private/configs/zii/v2ray/genrouter.json
    # tmuxnew v2ray-genrouter xray -config $nightNotes/private/configs/zii/v2ray/genrouter.json
}

function v2-off {
    ##
    # tmuxnew v2ray-genrouter v2ray -config $nightNotes/private/configs/zii/v2ray/direct.json
    ##
    tmuxnew v2ray-genrouter gost -L socks5://127.0.0.1:1081 -L http://127.0.0.1:1087
    ##
}
##
function httpify-socks5 {
    local socks="${1}"
    local http="${2}"

    reval-ec tmuxnew "httpify-socks-${socks}-to-${http}" gost -F "socks5://127.0.0.1:${socks}" -L "http://127.0.0.1:${http}"
}
##
function pxify {
    typeset -g proxycmd="proxychains4"
    typeset -g proxyenv="reval-pxa-if-no-proxy"

    pxaify-command tsend
    # enh-pxpy tsend

    pxaify-command llm
    pxaify-command gemini
    pxaify-command codex
    pxaify-command opencode

    pxaify-command doom

    pxaify-fn spotdl

    pxaify-command subgrab
    # enh-pxpy subgrab

    # keeping the shell bare-bones seem wiser
    pxify-command http # wget curl
    pxify-command kaggle
    pxify-command unalix
    pxify-command nimble # @broken
    # pxify-command go # TLS errors with genrouter
    pxify-command manga-py
    pxify-command pandoc # it needs this to download images

    pxaify-command brew # makes downloads faster
    pxaify-command pip # makes downloads faster
    pxify-command conda
    # pxaify-command git # use git's own config
    pxaify-command npm
    pxaify-command go
    pxaify-command dart

    # pxaify-command cargo # did not work
    # export CARGO_HTTP_PROXY=http://127.0.0.1:1087
    export CARGO_NET_GIT_FETCH_WITH_CLI=true

    # pxaify-command emacs
    # pxaify-command emacsclient
}
##
function pxify-command {
    aliasfn "$1" proxychains4 -q "$1"
    # -q: quiet
}
reify pxify-command

function pxaify-command {
    aliasfn "$1" pxa command "$1"
}
reify pxaify-command

function pxaify-fn {
    local name="$1"
    local name_new="h_pxa_${name}"
    assert-args name @RET
    assert not whitespace-p "${functions[$name]}" @RET

    functions[$name_new]="${functions[$name]}"
    functions[$name]="reval-pxa ${name_new} \"\$@\""
}
reify pxaify-fn
##
function pxpy {
    px python "$commands[$1]" "${@:2}"
}

function enh-pxpy {
    ge_no_hist=y geval "function $1() {
    pxpy $1 \"\$@\"
}"
}
##
function pxified-p {
    bool "${pxified}"
}

function should-proxy-p {
    #: This predicate can be used by programs to turn on a proxy by themselves.
    ##
    test -z "${proxy_disabled}${ALL_PROXY}${all_proxy}${HTTP_PROXY}${http_proxy}${HTTPS_PROXY}${https_proxy}${FTP_PROXY}${ftp_proxy}" && pxified-p
}

function proxy-auto-p {
    # local initCountry="$(serr mycountry)"
    if { isLocal && isMe } ; then # test -z "$initCountry" || [[ "$initCountry" == Iran ]] ; then
        return 0
    else
        return 1
    fi
}

function pxify-auto { # @gateway
    typeset -g pxified

    if { ! pxified-p } && proxy-auto-p ; then
        pxified=y
        pxify
    fi
}
##
function darwin-proxy-getns {
    networksetup -listnetworkserviceorder | rget '^\((?:\*|\d+)\)\s+(.*)$'
}

function darwin-proxy-getns-v1 {
    #: get the active network service
    #: from https://apple.stackexchange.com/a/223446/282215
    ##
    while read -r line; do
        sname=$(echo "$line" | awk -F  "(, )|(: )|[)]" '{print $2}')
        sdev=$(echo "$line" | awk -F  "(, )|(: )|[)]" '{print $4}')
        #echo "Current service: $sname, $sdev, $currentservice"
        if [ -n "$sdev" ]; then
            ifout="$(ifconfig "$sdev" 2>/dev/null)"
            echo "$ifout" | command rg 'status: active' > /dev/null 2>&1
            rc="$?"
            if [ "$rc" -eq 0 ]; then
                currentservice="$sname"
                currentdevice="$sdev"
                currentmac=$(echo "$ifout" | awk '/ether/{print $2}')

                # echo "$currentservice, $currentdevice, $currentmac"
                ec "$currentservice"
            fi
        fi
    done <<< "$(networksetup -listnetworkserviceorder | command rg 'Hardware Port')"

    if [ -z "$currentservice" ]; then
        >&2 echo "Could not find current service"
        exit 1
    fi
}

# aliasfn darwin-proxy-getns-cached memoi_expire=0 memoi-eval darwin-proxy-getns
aliasfn darwin-proxy-getns-cached darwin-proxy-getns #: no longer needs caching

function darwin-proxies-gen {
    local networks
    networks=(
        ${(@f)"$(darwin-proxy-getns-cached)"}

        ##
        #: having invalid/inactive entries here is okay
        # Wi-Fi
        # 'iPhone USB'
        # 'iPad USB'
        ##
    )

    excluded=(
        'Loopback'
        'FakeNet'
        'Sharif1'
        'eva-1'
        'VPN (Cisco IPSec)'
    )

    networks=(${(@)networks:|excluded})

    local ns
    for ns in ${(@u)networks} ; do
        ec "ns: $ns"
        eval-ec "$*"
    done
}

function darwin-proxies-get {
    darwin-proxies-gen networksetup -getsocksfirewallproxy '$ns'
}

function darwin-proxies-set {
    darwin-proxies-gen networksetup -setsocksfirewallproxy '$ns' "${2:-${ip:-localhost}}" "${1:-$socksport}"
}
aliasfnq darwin-proxies-on darwin-proxies-gen networksetup -setsocksfirewallproxystate '$ns' on
aliasfnq darwin-proxies-off darwin-proxies-gen networksetup -setsocksfirewallproxystate '$ns' off
##
aliasfnq darwin-dns-get darwin-proxies-gen networksetup -getdnsservers '$ns'
aliasfnq darwin-dns-set darwin-proxies-gen networksetup -setdnsservers '$ns'
##
function proxy-on {
    # proxy on
    # @darwinonly
    darwin-proxies-set
    darwin-proxies-on

    # proxy-widget-refresh
    btt-update $proxy_widget_uuid $proxy_widget_on
}

function proxy-off() {
    # proxy off
    # @darwinonly
    darwin-proxies-off

    # proxy-widget-refresh
    btt-update $proxy_widget_uuid $proxy_widget_off
}
##
function proxy-is() {
    unset proxy_port

    if isDarwin ; then
    local pxstatus="$(networksetup -getsocksfirewallproxy "$(darwin-proxy-getns-cached)")"

    [[ "$pxstatus" =~ 'Port:\s*(\S*)' ]] && proxy_port="$match[1]"

    dact ec "$pxstatus" >&2

    ec $pxstatus | silent command rg 'Enabled: Yes'
    else
        @NA
    fi
}

function proxy-toggle() {
    # silent bello &
    if proxy-is ; then
        proxy-off
    else
        proxy-on
    fi
}
##
proxy_widget_uuid=604BAF41-4517-4C56-B665-F1710C405A28
proxy_widget_on="🌋"
proxy_widget_on2="🏜"
proxy_widget_off="⛰"
function proxy-widget() {
    if proxy-is ; then
        case "$proxy_port" in
        $socksport) ec $proxy_widget_on ;;
        *) ec $proxy_widget_on2 ;;
        esac
    else
        ec $proxy_widget_off
    fi
}
# aliasfn proxy-widget-refresh btt-refresh '$proxy_widget_uuid'
proxy-widget-refresh() { btt-update $proxy_widget_uuid "$(proxy-widget)" }
##
function proxy-git-on {
    : "You need to set the appropriate SSH proxies, too"
    git config --global http.proxy socks5h://127.0.0.1:"${1:-1081}"
}

function proxy-git-off {
    git config --global --unset http.proxy
}
##
function proxy-restart {
    fz_opts=(-1) fftr gost-2083
    sleep 0.5
    ci98
}
alias '/'='proxy-restart'
alias '//'='reval-ec ping 8.8.8.8'
alias '/.'='reval-ec ci98'
##
function proxy-copy-env {
    local proxy_name="${1}"
    assert-args proxy_name @RET

    ec-copy "export ${aliases[${proxy_name}]}"
}
##
