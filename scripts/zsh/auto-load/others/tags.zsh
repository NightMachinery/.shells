function ntag-has() {
    local f="$1" tag="$2"
    
    [[ "$f" == *.${tag}.* ]]
}
function ntag-add() {
    local f="$1" tags=("${@:2}") tag toadd=()
    local ft="${f:t}"
    test -e "$f" || {
        ecerr "$0: Nonexistent file: $f"
        return 1
    }
    for tag in $tags[@] ; do
        if ! ntag-has "$ft" "$tag" ; then
            toadd+="$tag"
        fi
    done
    test -z "$toadd[*]" || reval-ec mv "$f" "${f:r}.${(j/./)toadd}.${f:e}"
}
alias tg=ntag-add
function ntag-get() {
    local tmp=("${(@0)$(<<<"$1" prefixer -i . -o '\x00' )}")
    local tags=( ${tmp[2,-2]} )
    arrN "$tags[@]"
}
function ntag-rm() {
    local f="$1" to_rm=("${@:2}") dest
    test -e "$f" || {
        ecerr "$0: Nonexistent file: $f"
        return 1
    }

    if [[ "$f" =~ '^([^.]*)(\..*)(\.[^.]*)$' ]] ; then
        dest="$(print -nr -- "$match[2]" | prefixer rm --skip-empty -i . -o . -- "$to_rm[@]")"
        if test -n "$dest" ; then
            dest="${match[1]}.${dest}${match[3]}"
        else
            dest="${match[1]}${match[3]}"
        fi
        if [[ "$f" != "$dest" ]] ; then
            reval-ec mv "$f" "$dest"
        fi
    fi
}
###
function ntag-toapple() {
    local f="$1"
    test -e "$f" || {
        ecerr "$0: Nonexistent file: $f"
        return 1
    }

    local tags=( "${(@f)$(ntag-get "$f")}" ) tag
    for tag in $tags[@] ; do
        case "${tag:l}" in
            red) reval-ec command tag --add Red "$f" ;;
            orange) reval-ec command tag --add Orange "$f" ;;
            yellow) reval-ec command tag --add Yellow "$f" ;;
            green) reval-ec command tag --add Green "$f" ;;
            blue) reval-ec command tag --add Blue "$f" ;;
            purple) reval-ec command tag --add Purple "$f" ;;
            gray|grey) reval-ec command tag --add Gray "$f" ;;
        esac
    done
}
##
function ntag-rm-colors() {
    ntag-rm "$1" red orange yellow green blue purple gray grey
}
function tag-apple-rm-colors() {
    command tag --remove Red,Orange,Yellow,Green,Blue,Purple,Gray "$@"
}
function tag-apple-get() {
    local f="$1"
    test -e "$f" || {
        ecerr "$0: Nonexistent file: $f"
        return 1
    }

    mdls -name kMDItemUserTags -raw "$f" -nullMarker '' | awk 'NR>1' | sed '$d' | prefixer --trim -i $',\n' --skip-empty
}
function ntag-fromapple() {
    local f="$1"
    test -e "$f" || {
        ecerr "$0: Nonexistent file: $f"
        return 1
    }

    ntag-add "$f" ${(@fL)"$(tag-apple-get "$f")"} # L lowercases
}
##
function ntag-rm-colors-rec() {
    @opts e ntag-rm-colors @ ntag-gen-rec "$@"
}
function tag-apple-rm-colors-rec() {
    @opts e tag-apple-rm-colors @ ntag-gen-rec "$@"
}
function ntag-fromapple-rec() {
    @opts e ntag-fromapple @ ntag-gen-rec "$@"
}
function ntag-toapple-rec() {
    @opts e ntag-toapple @ ntag-gen-rec "$@"
}
function ntag-gen-rec() {
    local engine=("${ntag_gen_rec_e[@]}")
    test -z "$engine[*]" && return 1
    local dir="${1:-.}"
    test -e "$dir" || return 1
    dir="$(realpath "$dir")"

    fd --no-ignore --hidden --type file --type symlink --type socket . "$dir" | inargsf re "$engine[@]"
    fd --no-ignore --hidden --type directory . "$dir" | inargsf re "$engine[@]" # @todo @BUG: Having nested tagged directories can invalidate the path of the deeper dir. Crude workaround: run this function multiple times. (Sorting by, e.g., length should solve this?)
    reval "$engine[@]" "$dir" # last because renaming the root dir will invalidate all further operations
}
##
function ntag-toapple-force() {
    tag-apple-rm-colors-rec
    ntag-toapple-rec
}
function ntag-fromapple-force() {
    ntag-rm-colors-rec
    ntag-fromapple-rec
}
## fuzzy
function ntag-search() {
    local query="$(mg_sep=' ' mapg "\'.\$i." "$@")"

    ##
    # local nightNotes="${ntag_search_dir:-.}"
    # ntsearch_glob='' ntsearch_rg_opts=(-uuu) ntl-fzf "$query"
    ##
    fd --no-ignore --hidden | fz --query "$query"
}
aliasfn tgs ntag-search
##
