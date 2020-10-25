##
socksport=1081
typeset -g proxycmd="${proxycmd:-command}"
typeset -g proxyenv="${proxyenv:-reval}"
alias pxs="ALL_PROXY=socks5://127.0.0.1:$socksport"
export pxa_env='ALL_PROXY=http://127.0.0.1:1087 http_proxy=http://127.0.0.1:1087 https_proxy=http://127.0.0.1:1087 HTTP_PROXY=http://127.0.0.1:1087 HTTPS_PROXY=http://127.0.0.1:1087'
alias pxa="$pxa_env"
alias pxa-local="local -x $pxa_env"
alias pxa-maybe='isIran && pxa-local'
function reval-pxa() {
    reval pxa "$@"
}
##
v2-on() {
    tmuxnew v2ray-genrouter v2ray -config $nightNotes/private/configs/zii/v2ray/genrouter.json
}
v2-off() {
    tmuxnew v2ray-genrouter v2ray -config $nightNotes/private/configs/zii/v2ray/direct.json
}
##
pxify() {
    typeset -g proxycmd="proxychains4"
    typeset -g proxyenv="reval-pxa"
    enh-pxpy tsend

    # keeping the shell bare-bones seem wiser
    pxify-command http # wget curl
    pxify-command conda
    pxify-command go
    pxify-command manga-py

    pxaify-command brew # makes downloads faster
    pxaify-command git
    pxaify-command emacs
    pxaify-command emacsclient
}
function pxify-command() {
    aliasfn "$1" proxychains4 "$1"
}
reify pxify-command
function pxaify-command() {
    aliasfn "$1" pxa command "$1"
}
reify pxaify-command
pxpy() {
    px python "$commands[$1]" "${@:2}"
}
enh-pxpy() {
    ge_no_hist=y geval "function $1() {
    pxpy $1 \"\$@\"
}"
}
function pxify-auto() { # @gateway
    # return 0 # We now use Wireguard
    typeset -g pxified
    # local initCountry="$(serr mycountry)"
    if test -z "$pxified" && isMBP ; then # test -z "$initCountry" || [[ "$initCountry" == Iran ]] ; then
        pxified=y
        pxify
    fi
}
silent pxify-auto
##
function darwin-proxy-getns() {
    # get the active network service
    # From https://apple.stackexchange.com/a/223446/282215
    
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
aliasfn darwin-proxy-getns-cached memoi_expire=0 memoi-eval darwin-proxy-getns
function darwin-proxies-gen() {
    local ns
    for ns in "$(darwin-proxy-getns-cached)" ; do # "${(@f)$(networksetup -listallnetworkservices | gsed 1d)}" ; do
        ec "ns: $ns"
        eval-ec "$*"
    done
}
aliasfnq darwin-proxies-set darwin-proxies-gen networksetup -setsocksfirewallproxy '$ns' localhost $socksport
aliasfnq darwin-proxies-on darwin-proxies-gen networksetup -setsocksfirewallproxystate '$ns' on
aliasfnq darwin-proxies-off darwin-proxies-gen networksetup -setsocksfirewallproxystate '$ns' off
##
function proxy-on() {
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
aliasfn pu proxy-on
aliasfn pd proxy-off
##
function proxy-is() {
    # @darwinonly
    unset proxy_port
    local pxstatus="$(networksetup -getsocksfirewallproxy "$(darwin-proxy-getns-cached)")"
    [[ "$pxstatus" =~ 'Port:\s*(\S*)' ]] && proxy_port="$match[1]"
    ec $pxstatus | silent command rg 'Enabled: Yes'
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
