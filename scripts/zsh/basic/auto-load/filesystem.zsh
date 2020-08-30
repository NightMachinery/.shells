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
function ensure-dir() {
    mkdir -p "$(bottomdir $1)"
}
reify ensure-dir
function lnrp() {
    local f="$1" d="$2"

    ln -s "$(realpath "$f")" "$d"
}
function rmdir-empty() {
    : "Removes all recursively empty directories from <root-dir>"

    local root="$1"
    if ! test -d "$root" ; then
        ecerr "$0: Non-existent root directory: $root"
        return 1
    fi
    # From https://unix.stackexchange.com/a/107556/282382
    gfind "$root" -mindepth 1 -type d -empty -delete
}
