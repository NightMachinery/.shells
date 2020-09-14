## Usage
# `fd [-uuu] ..tag..` or even `fd .tag.` easily lists the tagged files for you
# LIMITATION: The separator somewhat limits what chars you can use in a tag. For example, using `..`, we can't have the tag `.test.`, though `.te.st` is possible. I recommend against using  the sep chars at all, as it hurts readability, too.
##
ntag_colors=(red orange yellow green blue purple gray black aqua teal)
ntag_sep='..' # . is likely to conflict with existing names, but it's cute.
ntag_fd_opts=( --no-ignore --hidden ) # --no-ignore --hidden
## shortcut aliases
function h_tgfn_tag() {
    local tag="${1:? Tag required}"

    eval "function tg-$tag() {
    ntag-add \"\$1\" $tag
}
reify tg-$tag
"
}
re h_tgfn_tag $ntag_colors[@]
aliasfn mg tg-gray
function h_aliastag() {
    aliasfn "$1" ntag-filter "$1"
    @opts-setprefix "$1" ntag-search
}
re h_aliastag $ntag_colors[@]
aliasfnq gray ntag-filter "gray | 'grey"
@opts-setprefix gray ntag-search
aliasfn grey gray
@opts-setprefix grey ntag-search
##
function ntag-mv() {
    local i="$1" o="$2"
    color 100 255 200 "$0 $(gq "$@")" # >&2
    
    test -z "$o" && return 1
    test -e "$o" && {
        ecerr "$0: Dest exists: $o"
        return 1
    }
    mv "$i" "$o"
}
##
function ntag-migrate-sep() {
    local old="$1" new="$2" f="$3"
    { test -z "$old" || test -z "$new" } && return 1
    test -e "$f" || {
        ecerr "$0: Nonexistent file: $f"
        return 1
    }

    local ntag_sep="$old"
    local tags=( "${(@f)$(ntag-get "$f")}" )
    dvar tags
    
    ntag-rm "$f" "$tags[@]" || return 1
    ntag_sep="$new"
    ntag-add "$ntag_rm_dest" "$tags[@]" || {
        ecerr "$0: Reverting ..."
        ntag-mv "$ntag_rm_dest" "$f"
        return 1
    }
}
function ntag-migrate-sep-rec() {
    local old="$1" new="$2" fs=("${@:3}")

    ntag_gen_rec_e=(ntag-migrate-sep "$old" "$new") ntag-gen-rec "${fs[@]}"
}
##
function ntag_ftr() {
    local f="$1"

    local ft="${f:t}" # fe="${f:e}" fh="${f:h}"
    local ftr="${ft:r}"
    # test -z "$fe" && ftr="$ft" # :r strips the last dot even if the extension is empty
    # [[ "$ntag_sep" == *. ]] && ftr="${ftr}."
    [[ "${ftr}." == *"${ntag_sep}" ]] && ftr="${ftr}."
    # [[ "${ftr}" == '' ]] && ftr="${ntag_sep}"
    ec "$ftr"
}
function ntag_merge_dest_fe() {
    local dest="$1" fe="$2"

    if test -n "$fe" ; then
        if [[ "$dest" == *. ]] ; then
            dest="${dest}${fe}"
        else
            dest="${dest}.${fe}"
        fi
    fi

    ec "$dest"
}
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
    local ft="${f:t}" fe="${f:e}" fh="${f:h}"
    local ftr="$(ntag_ftr "$f")"

    for tag in $tags[@] ; do
        if ! ntag-has "$ft" "$tag" ; then
            toadd+="$tag"
        fi
    done
    test -z "$toadd[*]" || {
        local dest="$fh/$( {
              print -nr -- "$ftr" | prefixer --skip-empty -i "${ntag_sep}" -o '\x00'
              print -nr -- $'\0'
              arr0 $toadd[@]
               } | prefixer -i '\x00' -o "${ntag_sep}" )${ntag_sep}"
        dest="$(ntag_merge_dest_fe "$dest" "$fe")"
        ntag-mv "$f" "$dest" || return 1
    }
}
alias tg=ntag-add
function ntag-get() {
    : "You might want to use realpath before passing a path to this function. Since the tags might be stored on symlinks, we don't do that here automatically."

    local f="$1"
    local ft="${f:t}" fh="${f:h}" fe="${f:e}"
    local ftr="$(ntag_ftr "$f")"

    local tmp=("${(@0)$(<<<"$ftr" prefixer -i "${ntag_sep}" -o '\x00' )}")

    local tags=( ${tmp[2,-2]} )
    arrN "$tags[@]"
}
function ntag-rm() {
    : "GLOBAL OUT: ntag_rm_dest"
    unset ntag_rm_dest

    local f="$1" to_rm=("${@:2}") dest=''
    test -e "$f" || {
        ecerr "$0: Nonexistent file: $f"
        return 1
    }
    local ft="${f:t}" fh="${f:h}" fe="${f:e}"
    local ftr="$(ntag_ftr "$f")"

    local origparts="$(print -nr -- "${ftr}" | prefixer -i "${ntag_sep}" -o '\x00')"
    origparts=( "${(@0)origparts}" )
    local newparts=( "${(@)origparts[2,-1]:|to_rm}" )
    dest="${origparts[1]}${ntag_sep}$(arr0 "${newparts[@]}" |prefixer --skip-empty -i '\x00' -o "${ntag_sep}")${ntag_sep}"
    # dest="$(print -nr -- "${ftr}" | prefixer rm -i "${ntag_sep}" -o "${ntag_sep}" -- "$to_rm[@]")"
    re dvar ftr dest ntag_sep
    local parts="$(print -nr -- "${dest}" | prefixer --skip-empty -i "${ntag_sep}" -o '\x00')"
    parts=( "${(@0)parts}" )
    local parts_len="${#parts}"
    if (( parts_len == 1 )) ; then # [[ "$dest" =~ '^(.*)(\Q'"${ntag_sep}"'\E)$' ]] ; then
        dest="${parts[1]}"
    fi
    dest="$(ntag_merge_dest_fe "$dest" "$fe")"
    if test -n "$dest" && [[ "$ft" != "$dest" ]] ; then
        dest="${fh}/$dest"
        ntag_rm_dest="$dest"
        ntag-mv "$f" "$dest" || return 1
    else
        ntag_rm_dest="$f"
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
            # Uncomment this line to transfer all tags to the Apple tag system. Note that ntag-from-apple-force only removes colored tags currently, so you can not sync back custom tag removal from the Apple side.
            # *) reval-ec command tag --add "$tag" "$f" ;; # clutters things
        esac
    done
}
##
function ntag-rm-colors() {
    # Remove Apple color tags from the ntag system
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
    local engine_q="$(gq "$engine[@]")"
    local dir="${1:-.}"
    test -e "$dir" || return 1
    dir="$(realpath "$dir")"

    fd ${ntag_fd_opts[@]} --type file --type symlink --type socket . "$dir" | inargsf re "$engine_q"
    fd ${ntag_fd_opts[@]} --type directory . "$dir" | inargsf re "$engine_q" # @todo @BUG: Having nested tagged directories can invalidate the path of the deeper dir. Crude workaround: run this function multiple times. (Sorting by, e.g., length should solve this?)
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
    local orMode="${ntag_search_or}"
    local query_sep=''
    test -n "$orMode" && query_sep=' | '
    local query="$(mg_sep=' ' mapg "\'\${ntag_sep}\$i\${ntag_sep}\${query_sep}" "$@")"

    ##
    # local nightNotes="${ntag_search_dir:-.}"
    # ntsearch_glob='' ntsearch_rg_opts=(-uuu) ntl-fzf "$query"
    ##
    fd ${ntag_fd_opts[@]} | fzp "$query"
}
aliasfn tgsor @opts or y @ ntag-search
function tgs() {
    ntag-filter "$@" | fz --ansi
}
##
aliasfn ntag-grep fnswap isI false ntag-search
@opts-setprefix ntag-grep ntag-search
##
function ntag_filter_rg() {
    local pattern="$1" bg="${2:-0,0,0}" fg="${3:-255,255,255}"

    # We need to limit highlighting ntag_sep, as the color codes will impede further matches
    # `nobold`
    command rg --passthrough --smart-case --colors "match:none" --colors "match:style:bold" --fixed-strings --color always --colors "match:bg:$bg" --colors "match:fg:$fg" "${ntag_sep[-1]}${pattern}${ntag_sep[1]}"  #"${ntag_sep}${pattern}${ntag_sep}"
}
function ntag-filter() {
    : "Alt: Use ntag-grep if you never want the coloring."
    ## perf
    # `time (@opts or yes @ green red)` ~ 120ms
    # `time (fnswap isI false @opts or yes @ green red)` ~ 190ms
    ##
    local res
    res="$(ntag-grep "$@")" || return 1
    if isI ; then
        <<<$res ntag_filter_rg blue 0,0,255  | ntag_filter_rg green 0,255,0 0,0,0 | ntag_filter_rg red 255,0,0 | ntag_filter_rg orange 255,120,0 | ntag_filter_rg yellow 255,255,0 0,0,0 | ntag_filter_rg purple 100,10,255 | ntag_filter_rg gray 100,100,100 | ntag_filter_rg grey 100,100,100 | ntag_filter_rg black 0,0,0 | ntag_filter_rg aqua 0,255,255 0,0,0 | ntag_filter_rg teal 0,128,128 | command rg --passthrough --smart-case --colors "match:none" --colors "match:style:bold" --color always --colors "match:bg:255,255,255" --colors "match:fg:255,120,0" '(?<!;\dm)(?<!;\d\dm)(?<!;\d\d\dm)\.(?!\e\[)[^./]+\.' --pcre2
        # https://www.regular-expressions.info/lookaround.html
        # Lookbehind needs to be fixed length in rg's pcre.
        # Use `cat -v` to see the ANSI codes. `\e` is `^[[`.
    else
        ec $res
    fi
}
@opts-setprefix ntag-filter ntag-search
aliasfn tgf ntag-filter
aliasfn tgfor @opts or y @ ntag-filter
##
