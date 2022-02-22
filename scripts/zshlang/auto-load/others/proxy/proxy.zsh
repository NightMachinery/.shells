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
alias pxs-maybe='isLocal && pxs-local'
function reval-pxs() {
    reval pxs "$@"
}
##
function pxa-create() {
    typeset -g px_httpport="${1:-1087}"
    # 1087: genrouter
    # 1088: shadowsocks
    # @todo0 make this support multiple ports at the  same time

    export pxa_env="ALL_PROXY=http://127.0.0.1:${px_httpport} http_proxy=http://127.0.0.1:${px_httpport} https_proxy=http://127.0.0.1:${px_httpport} HTTP_PROXY=http://127.0.0.1:${px_httpport} HTTPS_PROXY=http://127.0.0.1:${px_httpport}"
    silent re unalias pxa pxa-local
    alias pxa="$pxa_env"
    alias pxa-local="local -x $pxa_env"
}
pxa-create

# alias pxa-maybe='isIran && pxa-local'
alias pxa-maybe='isLocal && pxa-local'
function reval-pxa() {
    pxa reval "$@"
}
##
function v2-on() {
    tmuxnew v2ray-genrouter v2ray -config $nightNotes/private/configs/zii/v2ray/genrouter.json
}

function v2-off() {
    tmuxnew v2ray-genrouter v2ray -config $nightNotes/private/configs/zii/v2ray/direct.json
}
##
function pxify() {
    typeset -g proxycmd="proxychains4"
    typeset -g proxyenv="reval-pxa"

    enh-pxpy tsend
    # enh-pxpy subgrab

    pxaify-fn spotdl

    pxaify-command subgrab

    # keeping the shell bare-bones seem wiser
    pxify-command http # wget curl
    pxify-command unalix
    pxify-command nimble # @broken
    pxify-command conda
    # pxify-command go # TLS errors with genrouter
    pxify-command manga-py
    pxify-command pandoc # it needs this to download images

    pxaify-command brew # makes downloads faster
    pxaify-command pip # makes downloads faster
    # pxaify-command git # use git's own config
    pxaify-command npm
    pxaify-command go
    pxaify-command dart

    # pxaify-command cargo # did not work, using CARGO_HTTP_PROXY instead
    export CARGO_HTTP_PROXY=socks5://localhost:1078

    pxaify-command emacs
    pxaify-command emacsclient
}
##
function pxify-command() {
    aliasfn "$1" proxychains4 -q "$1"
    # -q: quiet
}
reify pxify-command

function pxaify-command() {
    aliasfn "$1" pxa command "$1"
}
reify pxaify-command

function pxaify-fn() {
    local name="$1"
    local name_new="h_pxa_${name}"
    assert-args name @RET
    assert not whitespace-p "${functions[$name]}" @RET

    functions[$name_new]="${functions[$name]}"
    functions[$name]="reval-pxa ${name_new} \"\$@\""
}
reify pxaify-fn
##
function pxpy() {
    px python "$commands[$1]" "${@:2}"
}

function enh-pxpy() {
    ge_no_hist=y geval "function $1() {
    pxpy $1 \"\$@\"
}"
}
##
function proxy-auto-p {
    # local initCountry="$(serr mycountry)"
    if test -z "$pxified" && { isMBP } ; then # test -z "$initCountry" || [[ "$initCountry" == Iran ]] ; then
        return 0
    else
        return 1
    fi
}

function pxify-auto() { # @gateway
    typeset -g pxified

    if proxy-auto-p ; then
        pxified=y
        pxify
    fi
}
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

function darwin-proxies-get {
    darwin-proxies-gen networksetup -getsocksfirewallproxy '$ns'
}

function darwin-proxies-set() {
    darwin-proxies-gen networksetup -setsocksfirewallproxy '$ns' localhost "${1:-$socksport}"
}
aliasfnq darwin-proxies-on darwin-proxies-gen networksetup -setsocksfirewallproxystate '$ns' on
aliasfnq darwin-proxies-off darwin-proxies-gen networksetup -setsocksfirewallproxystate '$ns' off
##
aliasfnq darwin-dns-get darwin-proxies-gen networksetup -getdnsservers '$ns'
aliasfnq darwin-dns-set darwin-proxies-gen networksetup -setdnsservers '$ns'
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
function proxy-git-on() {
    : "You need to set the appropriate SSH proxies, too"
    git config --global http.proxy socks5h://127.0.0.1:"${1:-1081}"
}
function proxy-git-off() {
    git config --global --unset http.proxy
}
##
