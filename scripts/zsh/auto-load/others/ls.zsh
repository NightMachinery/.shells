function onla() {
    geval "$(gquote "$@")"" ${$(last-added):q}"
}
function onxla(){
    last-added|gxargs -I _ "$(gquote "$@")"
}
function onxlc(){
    last-created|gxargs -I _ "$(gquote "$@")"
}
function first-file(){
    exa|head -n1
}
function onlac(){
    geval "$(gquote "$@")"" ${$(last-accessed):q}"
}
function onlm(){
    geval "$(gquote "$@")"" ${$(last-modified):q}"
}
function onlc(){
    geval "$(gquote "$@")"" ${$(last-created):q}"
}
function onff(){
    geval "$(gquote "$@")"" ${$(first-file):q}"
}
