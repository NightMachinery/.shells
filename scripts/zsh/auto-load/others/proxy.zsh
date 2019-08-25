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
