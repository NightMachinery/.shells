##
function wayback() {
    comment -e, --exact-url
    comment "-t, --to TIMESTAMP Should take the format YYYYMMDDhhss, though
                        you can omit as many of the trailing digits as you
                        like. E.g., '201501' is valid."
    comment '-p, --maximum-snapshot NUMBER    Maximum snapshot pages to consider (Default is 100)'
    comment '-d, --directory PATH             Directory to save the downloaded files into
    Default is ./websites/ plus the domain name'

    wayback_machine_downloader -e -t "${wa_t:-20170505152803}" -d ./ -p 1 "$@"
}

wayback-out() {
    aget "wayback $(gquote "$@") ; cat *.html"
}

wread-wayback() {
    wayback-out "$1" | wread --file /dev/stdin "$@"
}

function wayback-url() {
    # --to-date "${wa_t:-2017}" --from-date 2000
    # outputs from oldest to newest
    waybackpack --list "$1" | sponge | head -n1
}
# enh-urlfinal wayback-url ## old URLs often redirect to hell
reify wayback-url
noglobfn wayback-url
##
function url-date-wayback() {
    local url="$1"

    local first
    first="$(wayback-url "$url")" # || return 1

    # YYYYMMDDhhmmss
    if [[ "$first" =~ 'https://web.archive.org/web/(\d{4})(\d{2})(\d{2})\d*/' ]] ; then
        # gdate --date "$match[1]" "+%F"
        ec "${match[1]}-${match[2]}-${match[3]}"
    fi
}
##
