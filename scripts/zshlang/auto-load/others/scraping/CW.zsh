##
function aaCW() {
    mdoc "$0 <url, 1 <= level of recursion <= 2 > ..." MAGIC

    local theCookies=${theCookies:-"$(cookies $1)"} fhMode=aacookies
    getlinks-c -e 'resource/view\.php' "$@" | inargsf aamedia
}
noglobfn aaCW
alias aaCW1='aamedia1'
##
