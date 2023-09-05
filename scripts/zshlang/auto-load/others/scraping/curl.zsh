##
function curl-stdout {
    curlm -o /dev/stdout "$@"
}
aliasfn gurl curl-stdout
##
function curl-remotename {
    #: -O, --remote-name          Write output to a file named as the remote file
    #: -J, --remote-header-name   Use the header-provided filename

    curlm --remote-name --remote-header-name "$@"
}
##
function curl-dns-cook {
    local dns_server="${NIGHT_DNS:-1.1.1.1}"
    local urls=()

    local arg url url_domain ip_address
    for arg in "$@"; do
        if match-url "$arg"; then
            url="$arg"
            url_domain="$(url-domain "$url")" @TRET
            ip_address="$(dig +short @${dns_server} "${url_domain}")"

            if [ -n "$ip_address" ]; then
                urls+=(
                    "--resolve" "${url_domain}:443:$ip_address"
                    "--resolve" "${url_domain}:80:$ip_address"
                )
            else
                ecerr "$0: Failed to resolve IP address for $url using DNS server $dns_server"
                return 1
            fi
        fi
    done

    local cmd
    cmd=(curl "${urls[@]}" "$@")
    pbcopy-z "${cmd[@]}"
}
##
