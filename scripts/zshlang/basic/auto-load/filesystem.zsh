function realpath-relchild {
    local dir="$1" mypath="$2"

    local rel="$(grealpath --relative-to "$dir" -- "$mypath")"
    if [[ "$rel" =~ '^../' ]] ; then
        grealpath -- "$mypath"
    else
        ec "$rel"
    fi
}
##
function cdm {
    local d="${*}"
    if test -z "$d" ; then
        ecerr "$0: empty input!"
        ectrace
        return 1
    fi

    mkdir-m "$d" &&
        cd -P -- "$d"
}
##
function bottomdir() {
    # I have tried to output the dir without its trailing '/'
    ##
    local empty="${bottomdir_empty-.}"

    local out
    if [ -e "$1" ] && ! [ -d "$1" ] ; then
        out="${1:h}"
    else
        if [[ "$1" =~ '^(.+)/$' ]] ; then
            out="$match[1]"
        elif [ -e "$1" ] ; then
            out="$1"
        elif [[ "$1" == */* ]] ; then
            out="${1:h}"
        else
            out="$empty"
        fi
    fi

    ec "${out}"
    ## @bugs
    # `bottomdir ./non-existent/..`
    #   (The same bug exists in 'bottomfile'.)
    ##
}

function bottomfile() {
    local name="$1"

    if test -n "$name" && ! test -d "$name" ; then
        local dir="$(bottomdir "$name")"
        ecn "$name" | prefixer -r "${dir}" | sd '^/*' ''
    fi
    ## @bugs
    # `bottomfile /non-existent/..`
    #   It's hard to solve these bugs, and this usage is not really intended.
    ##
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
function mkdir-m {
    local d
    for d in $@ ; do
        mkdir -p -- "$d"
    done
}

function ensure-dir {
    mkdir -p -- "$(bottomdir $1)"
}
reify ensure-dir
##
function lnrp-re {
    local fs=("${@}") d="${@[-1]}"

    local f
    for f in ${fs[@]} ; do
        lnrp "$f" "$d"
    done
}

function lnrp {
    local f="${1}" d="${2}" opts=( "${@[3,-1]}" )
    assert-args f d @RET

    local i
    if ! i="$(realpath2 "$f")" ; then
        ectrace_ret=1 ectrace "$0: failed to get the realpath of $(gquote-sq "$f")"
        return $?
    fi

    if bool "${lnrp_auto_i:-y}" && isIReally && isRcLoaded && fn-isTop lnrp lnrp-re ; then
        opts+=( -i )
    fi

    reval-ec ln "$opts[@]" -s "$i" "$d"
}
##
function rmdir-empty() {
    : "Removes all recursively empty directories from <root-dir>"
    : "@alt [agfi:rm-empty]"

    local root="$1"
    if ! test -d "$root" ; then
        ecerr "$0: Non-existent root directory: $root"
        return 1
    fi
    # From https://unix.stackexchange.com/a/107556/282382
    gfind "$root" -mindepth 1 -type d -empty -print -delete
}
##
function append-f2f {
    local from="$(grealpath -- "$1")" to="$(grealpath --canonicalize-missing -- "$2")"
    if [[ "${from:l}" == "${to:l}" ]] ; then # grealpath --canonicalize-missing does not normalize the case in macOS, so we are forcing them both to lowercase.
        ecerr "$0: Destination is the same as the source. Aborting."
        return 1 # We rely on this not being zero
    fi
    ensure-dir "$to"
    cat "$from" | sponge -a "$to"
}
##
function mv-merge {
    ##
    # https://unix.stackexchange.com/questions/127712/merging-folders-with-mv/172402
    # https://unix.stackexchange.com/questions/654481/gnu-cp-whats-the-difference-between-link-and-reflink-always
    ##
    # * Known bugs:
    # ** =mv-merge some_dir . = will delete some_dir.
    ##
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
    assert cp-link --verbose "${opts[@]}" "$@" >&2 || return $? #  --link option of the cp command, which creates hard links of files on the same filesystem instead of full-data copies. --archive preserve all metadata

    { colorfg "$gray[@]" ; trs "${(@)paths[1,-2]}" ; resetcolor } >&2
}

function cp-link {
    reval-ec command gcp -r --link --archive --verbose "$@"
}
aliasfn cpl cp-link
##
function fd-exists() {
    fd -uu --ignore-case --max-results=1 "$@" | silent command rg .
    # https://github.com/sharkdp/fd/issues/303
}
function fd-exists-d1() {
    fd-exists --max-depth=1 "$@"
}
##
function dest-overwrite-p {
    local dests=("$@") interactive="${dest_overwrite_p_i}" overwrite_by_default="${dest_overwrite_p_default:-y}" force_overwrite="${dest_overwrite_p_f}"

    if bool "$force_overwrite" ; then
        return 0
    fi

    local f
    for f in $dests[@] ; do
        if test -s "$f" ; then # file exists and has a size greater than zero
            if test -z "$interactive" ; then
                if isIReally ; then
                    interactive=y
                fi
            fi

            if bool "$interactive" ; then
                if ask "$0: Overwrite $(gquote-sq "$f")?" N ; then
                    return 0
                else
                    return 1
                fi
            else
                if bool "$overwrite_by_default" ; then
                    return 0
                else
                    return 1
                fi
            fi
        fi
    done
}
##
function cp-tmp {
    local files=($@)

    local file tmp
    for file in $files[@] ; do
        tmp="$(gmktemp --suffix=".${file:e}")" @TRET
        cp "$file" "$tmp" >&2 @TRET

        ec "$tmp"
    done
}
##
