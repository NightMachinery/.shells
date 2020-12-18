function realpath-relchild() {
    local dir="$1" mypath="$2"

    local rel="$(realpath --relative-to "$dir" "$mypath")"
    if [[ "$rel" =~ '^../' ]] ; then
        realpath "$mypath"
    else
        ec "$rel"
    fi
}
##
function cdm() {
    local d="$*"

    mkdir -p -- "$d" &&
        cd -P -- "$d"
}
function bottomdir() {
    { { [ -e "$1" ]  && ! [ -d "$1" ] } || { ! [ -e "$1" ] && [[ "$1" != */ ]] } } && { ec "${1:h}"; } || { ec "$1"; } ;}
function cdd() {
    cd "$(bottomdir "$1")"
}
function cdz() {
    local i="$*"

    if test -d "$i" ; then
        cd "$i"
    else
        ffz "$i"
    fi
}
##
function ensure-dir() {
    mkdir -p "$(bottomdir $1)"
}
reify ensure-dir
function lnrp() {
    local f="$1" d="$2"

    ln -s "$(realpath2 "$f")" "$d"
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
function append-f2f() {
    local from="$(realpath "$1")" to="$(realpath --canonicalize-missing "$2")"
    if [[ "${from:l}" == "${to:l}" ]] ; then # realpath --canonicalize-missing does not normalize the case in macOS, so we are forcing them both to lowercase.
        ecerr "$0: Destination is the same as the source. Aborting."
        return 1 # We rely on this not being zero
    fi
    ensure-dir "$to"
    cat "$from" | sponge -a "$to"
}
