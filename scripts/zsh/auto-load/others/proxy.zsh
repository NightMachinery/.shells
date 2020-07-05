pxify() {
    enh-pxpy tsend
}
pxpy() {
    px python "$commands[$1]" "${@:2}"
}
enh-pxpy() {
    ge_no_hist=y geval "function $1() {
    pxpy $1 \"\$@\"
}"
}
function pxify-auto() {
    # local initCountry="$(serr mycountry)"
    if [[ "$(hostname)" == 'Fereidoons-MacBook-Pro.local' ]] ; then # test -z "$initCountry" || [[ "$initCountry" == Iran ]] ; then
        pxify
    fi
}
