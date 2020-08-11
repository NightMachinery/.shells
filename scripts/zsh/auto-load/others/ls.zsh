function ls-by-added() {
    # Doesn't work well for files having '(null)' as their DateAdded, which some do.
    mdls -name kMDItemFSName -name kMDItemDateAdded -raw -- *(D) | \
        xargs -0 -I {} echo {} | \
        sed 'N;s/\n/ /' | \
        sort --reverse | \
        sed -E "s/^.*\\+0000 //" # removes the timestamps
}
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
