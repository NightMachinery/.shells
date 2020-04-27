cdm() {
    mkdir -p -- "$1" &&
        cd -P -- "$1"
}
function bottomdir() {
    { { [ -e "$1" ]  && ! [ -d "$1" ] } || { ! [ -e "$1" ] && [[ "$1" != */ ]] } } && { ec "${1:h}"; } || { ec "$1"; } ;}
function cdd() {
    cd "$(bottomdir "$1")" }
trs() {
    local i
    for i in "$@"
    do
        [[ -e "$i" ]] && {
            ec Trying to remove "$i"
            trash -- "$i"
        }
    done
}
