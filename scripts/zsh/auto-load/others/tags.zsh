##
ntag_sep=',,'
##
function ntag-has() {
    local f="$1" tag="$2"
    
    [[ "$f" == *"${ntag_sep}${tag}${ntag_sep}"* ]]
}
function ntag-add() {
    local f="$1" tags=("${@:2}") tag toadd=()
    test -e "$f" || {
        ecerr "$0: Nonexistent file: $f"
        return 1
    }
    local ft="${f:t}" fe="${f:e}"
    local ftr="${ft:r}"

    for tag in $tags[@] ; do
        if ! ntag-has "$ft" "$tag" ; then
            toadd+="$tag"
        fi
    done
    test -z "$toadd[*]" || {
        local dest="$( {
              print -nr -- "$ftr" | prefixer -i "${ntag_sep}" -o '\x00'
              print -nr -- $'\0'
              arr0 $toadd[@]
               } | prefixer --skip-empty -i '\x00' -o "${ntag_sep}" )${ntag_sep}"
        if test -n "$fe" ; then
            dest="${dest}.${fe}"
        fi
        reval-ec mv "$f" "$dest"
    }
}
alias tg=ntag-add
function ntag-get() {
    : "You might want to use realpath before passing a path to this function. Since the tags might be stored on symlinks, we don't do that here automatically."

    local input="${1:r}"
    local tmp=("${(@0)$(<<<"$input" prefixer -i "${ntag_sep}" -o '\x00' )}")

    local tags=( ${tmp[2,-2]} )
    arrN "$tags[@]"
}
function ntag-rm() {
    local f="$1" to_rm=("${@:2}") dest=''
    test -e "$f" || {
        ecerr "$0: Nonexistent file: $f"
        return 1
    }
    local ft="${f:t}" fh="${f:h}" fe="${f:e}"
    local ftr="${ft:r}"

    # if [[ "$ft" =~ '^([^.]*)(\..*)(\.[^.]*)$' ]] ; then
    #     dest="$(print -nr -- "$match[2]" | prefixer rm --skip-empty -i . -o . -- "$to_rm[@]")"
    #     if test -n "$dest" ; then
    #         dest="${match[1]}.${dest}${match[3]}"
    #     else
    #         if [[ "${match[3]}" != '.' ]] ; then
    #             dest="${match[1]}${match[3]}"
    #         else
    #             dest="${match[1]}" # No empty extension
    #         fi
    #     fi
    # fi

    # We should add the dot removed from ftr (Can thus cause a bug if there is no extension?)
    dest="$(print -nr -- "${ftr}" | prefixer rm -i "${ntag_sep}" -o "${ntag_sep}" -- "$to_rm[@]")"
    re dvar ftr dest
    local parts="$(print -nr -- "${dest}" | prefixer --skip-empty -i "${ntag_sep}" -o '\x00')"
    parts=( "${(@0)parts}" )
    local parts_len="${#parts}"
    if (( parts_len == 1 )) ; then # [[ "$dest" =~ '^(.*)(\Q'"${ntag_sep}"'\E)$' ]] ; then
        dest="${parts[1]}"
    fi
    if test -n "$fe" ; then
    dest="${dest}.${fe}"
    fi
    # fi
    if test -n "$dest" && [[ "$ft" != "$dest" ]] ; then
        dest="${fh}/$dest"
        reval-ec mv "$f" "$dest"
    fi
}
###
function ntag-toapple() {
    local f="$1"
    test -e "$f" || {
        ecerr "$0: Nonexistent file: $f"
        return 1
    }
    local ft="${f:t}" fh="${f:h}"

    local tags=( "${(@f)$(ntag-get "$ft")}" ) tag
    for tag in $tags[@] ; do
        case "${tag:l}" in
            red) reval-ec command tag --add Red "$f" ;;
            orange) reval-ec command tag --add Orange "$f" ;;
            yellow) reval-ec command tag --add Yellow "$f" ;;
            green) reval-ec command tag --add Green "$f" ;;
            blue) reval-ec command tag --add Blue "$f" ;;
            purple) reval-ec command tag --add Purple "$f" ;;
            gray|grey) reval-ec command tag --add Gray "$f" ;;
            *) reval-ec command tag --add "$tag" "$f" ;;
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
    local query="$(mg_sep=' ' mapg "\'\${ntag_sep}\$i\${ntag_sep}" "$@")"

    ##
    # local nightNotes="${ntag_search_dir:-.}"
    # ntsearch_glob='' ntsearch_rg_opts=(-uuu) ntl-fzf "$query"
    ##
    fd --no-ignore --hidden | fz --query "$query"
}
aliasfn tgs ntag-search
##
