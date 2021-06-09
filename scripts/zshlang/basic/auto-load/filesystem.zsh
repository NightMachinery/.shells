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
##
function bottomdir() {
    { { [ -e "$1" ]  && ! [ -d "$1" ] } || { ! [ -e "$1" ] && [[ "$1" != */ ]] } } && { ec "${1:h}"; } || { ec "$1"; }
}
function bottomfile() {
    local name="$1"

    if test -n "$name" && ! test -d "$name" ; then
        local dir="$(bottomdir "$name")"
        ecn "$name" | prefixer -r "${dir}" | sd '^/*' ''
    fi
}
##
function dir-rmprefix() {
    local dir="$1" ; shift
    assert-args dir @RET

    prefixer --case-sensitivity no -r "$dir" "$@" | sd '^/*' ''
}
##
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
    local f="${1:?}" d="${2:?}"

    local i
    i="$(realpath2 "$f")" @TRET

    ln -i -s "$i" "$d"
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
##
function  mv-merge() {
    # https://unix.stackexchange.com/questions/127712/merging-folders-with-mv/172402
    local paths=() i opts_end
    for i in "$@" ; do
        if [[ -n "$opts_end" || "$i" != '-'* ]] ; then
            paths+="$i"
        fi
        if [[ "$i" == '--' ]] ; then
            opts_end=y
        fi
        if [[ "$(grealpath --  "$i")" == /Volumes/* ]] ; then
            if ask "$0: There seems to be external (cross-device) paths in args. Proceed with using normal mv instead?" Y ; then
                command gmv -i --verbose "$@" >&2
                return $?
            fi
        fi
    done
    if (( ${#paths} <= 1 )) ; then
        ecerr "$0: Only one path supplied."
        return 1
    fi
    local opts=()
    isIReally && opts+='--interactive'
    assert command gcp -r --link --archive --verbose "${opts[@]}" "$@" >&2 || return $? #  --link option of the cp command, which creates hard links of files on the same filesystem instead of full-data copies. --archive preserve all metadata

    { colorfg "$gray[@]" ; trs "${(@)paths[1,-2]}" ; resetcolor } >&2
}
##
function fd-exists() {
    fd -uu --ignore-case --max-results=1 "$@" | silent command rg .
    # https://github.com/sharkdp/fd/issues/303
}
function fd-exists-d1() {
    fd-exists --max-depth=1 "$@"
}
##
