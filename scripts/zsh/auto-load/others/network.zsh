function ncp() {
    cat | gnc -c localhost 2000
}
function ip-internal-get() {
    : "Use 'ipconfig getifaddr en1' for wireless, or 'ipconfig getifaddr en0' for ethernet."

    ifconfig | grep "inet "
}
##
function http-static-py() {
    python -m http.server "${1:-8000}"
}
function http-static-caddy() {
    caddy file-server -browse -listen "0.0.0.0:${1:-8000}"
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
function ip-geolocate() {
    gurl ipinfo.io/"$1"/geo
}
