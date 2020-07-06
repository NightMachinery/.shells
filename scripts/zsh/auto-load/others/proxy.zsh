pxify() {
    enh-pxpy tsend

    # keeping the shell bare-bones seem wiser
    pxify-command http # wget curl
}
function pxify-command() {
    aliasfn "$1" proxychains4 "$1"
}
reify pxify-command
pxpy() {
    px python "$commands[$1]" "${@:2}"
}
enh-pxpy() {
    ge_no_hist=y geval "function $1() {
    pxpy $1 \"\$@\"
}"
}
function pxify-auto() {
    typeset -g pxified
    # local initCountry="$(serr mycountry)"
    if test -z "$pxified" && [[ "$(hostname)" == 'Fereidoons-MacBook-Pro.local' ]] ; then # test -z "$initCountry" || [[ "$initCountry" == Iran ]] ; then
        pxified=y
        pxify
    fi
}
silent pxify-auto
