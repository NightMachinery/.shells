## Usage
# `fd [-uuu] ..tag..` or even `fd .tag.` easily lists the tagged files for you
# LIMITATION: The separator somewhat limits what chars you can use in a tag. For example, using `..`, we can't have the tag `.test.`, though `.te.st` is possible. I recommend against using  the sep chars at all, as it hurts readability, too.
##
# macOS deps:
# https://github.com/jdberry/tag
##
ntag_colors=(red orange yellow green blue purple gray black aqua teal)
ntag_sep='..' # . is likely to conflict with existing names, but it's cute.
ntag_fd_opts=( --no-ignore --hidden ) # --no-ignore --hidden
### shortcut aliases
function h_tgfn_tag() {
    local tag="${1:? Tag required}"

    aliasfnq tg-"$tag" ntag-add-multi "$tag"
}
re h_tgfn_tag $ntag_colors[@]
aliasfn mg tg-gray
##
function ntag-filter-or-add() {
    local tag="${1:?}" ; shift
    if (( $#@ == 0 )) ; then
        ntag-filter "$tag"
    else
        ntag-add-multi "$tag" "$@"
    fi
}
function h_aliastag() {
    # aliasfn "$1" ntag-filter "$1" # @altAPT ; This is used less often, so I dropped it. You can use `tgf blue green ...` for this.
    aliasfn "$1" ntag-filter-or-add "$1"
    @opts-setprefix "$1" ntag-search
}
re h_aliastag $ntag_colors[@] @todo @todo{0..9} # You can use `fd ..@todo` to prefix-search.
##
# aliasfnq gray ntag-filter "gray | 'grey"
function gray() {
    local tag="gray | 'grey"
    if (( $#@ == 0 )) ; then
        ntag-filter "$tag"
    else
        ntag-add-multi "gray" "$@"
    fi
}
@opts-setprefix gray ntag-search
aliasfn grey gray
@opts-setprefix grey ntag-search
###
function ntag-l() {
    if isI && istty ; then
        exa -a --color always "$@" | ntag-color | rtl-reshaper
    else
        exa -a "$@"
    fi
}
aliasfn l ntag-l
aliasfn lv-simple ntag-l -- '${~videoglob}'
aliasfn lv lv-simple2
lv-sorted() { ntag-lv | tac }
lv-simple2() { @opts nopriority y @ ntag-lv | tac }
function ntag-ll() {
    if isI && istty ; then
        exa -a -l --color always "$@" | ntag-color | rtl-reshaper
    else
        exa -a -l "$@"
    fi
}
aliasfn ll ntag-ll
# aliasfn lll ntag-ll
function ntag-lt() {
    if isI && istty ; then
        exa -a -T --color always "$@" | ntag-color | rtl-reshaper
    else
        exa -a -T "$@"
    fi
}
aliasfn lt ntag-lt
##
ntag-ls() {
    : "Lists only tagged files"
    local paths=("${@:-.}") # you can also give options to fd, e.g., `ntag-ls --maxdepth 1`

    if isI && istty ; then
        fd --color always ${ntag_fd_opts[@]} --glob --type file "*..*..*" "$paths[@]" | ntag-color | rtl-reshaper
    else
        fd ${ntag_fd_opts[@]} --glob --type file "*..*..*" "$paths[@]"
    fi
}
aliasfn lk ntag-ls
function ntag-ls-head() {
    ntag-ls --color always | prefixer -a "${PWD:t}/"
}
aliasfn lkh ntag-ls-head
##
function ntag-rmadd() {
    ## tests
    # `@opts rm [ red bad ] add [ yellow blue purple ] @ ntag-rmadd `
    ##
    local files=("$@") f retcode=0
    local add=("${ntag_rmadd_add[@]}")
    local rm=("${ntag_rmadd_rm[@]}")

    if (( $#@ == 0 )) ; then
        files=("${(@f)$(ntag-grepor ${rm[@]} | ntag-color | fz-ntag)}") || return $?
    fi

    for f in $files[@] ; do
        test -e "$f" || {
            ecerr "$0: Nonexistent file: $f"
            retcode=1
            continue
        }

        ntag-rm "$f" $rm[@] || {
            retcode=1
            continue
        }
        ntag-add "$ntag_rm_dest" $add[@]
        ntag-toapple-force "$ntag_add_dest"
    done
    return $retcode
}
function ntag-add-multi() {
    @opts add "${1:?}" @ ntag-rmadd "${@:2}"
}
aliasfn tgm ntag-add-multi
###
aliasfn green2red @opts rm green add red @ ntag-rmadd
aliasfn green2gray @opts rm green add gray @ ntag-rmadd
aliasfn green2teal @opts rm green add teal @ ntag-rmadd
aliasfn green2aqua @opts rm green add aqua @ ntag-rmadd
greens=( green aqua teal )
aliasfn greens2red @opts rm [ "$greens[@]" ] add red @ ntag-rmadd
aliasfn greens2gray @opts rm [ "$greens[@]" ] add gray @ ntag-rmadd
aliasfn greens2teal @opts rm [ "$greens[@]" ] add teal @ ntag-rmadd
aliasfn greens2aqua @opts rm [ "$greens[@]" ] add aqua @ ntag-rmadd
aliasfn greens2hell @opts rm [ "$greens[@]" ] @ ntag-rmadd
##
aliasfn red2hell @opts rm [ red ] @ ntag-rmadd
###
function ntag-mv() {
    local i="$1" o="$2"
    color 100 255 200 "$0 $(gq "$@")" # >&2
    
    test -z "$o" && return 1
    test -e "$o" && {
        ecerr "$0: Dest exists: $o"
        return 1
    }
    command mv "$i" "$o"
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
function ntag-add-givedest() {
    silent ntag-add "$@" || {
        ec "${1}"
        return $?
    }
    ec "$ntag_add_dest"
}
function ntag-add() {
    : "GLOBAL OUT: ntag_add_dest"
    unset ntag_add_dest

    local f="$(ntag-recoverpath "$1")" tags=("${(u)@:2}") tag toadd=()
    ntag_add_dest="$f"
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
        ntag-toapple-force "$dest"
        ntag_add_dest="$dest"
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
        ntag-toapple-force "$dest"
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
            red) revaldbg command tag --add Red "$f" ;;
            orange) revaldbg command tag --add Orange "$f" ;;
            yellow) revaldbg command tag --add Yellow "$f" ;;
            green) revaldbg command tag --add Green "$f" ;;
            blue) revaldbg command tag --add Blue "$f" ;;
            purple) revaldbg command tag --add Purple "$f" ;;
            gray|grey) revaldbg command tag --add Gray "$f" ;;
            # Uncomment this line to transfer all tags to the Apple tag system. Note that ntag-from-apple-force only removes colored tags currently, so you can not sync back custom tag removal from the Apple side.
            # *) revaldbg command tag --add "$tag" "$f" ;; # clutters things
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

    ##
    # https://apple.stackexchange.com/questions/401225/mdls-does-not-work-on-mounted-sparse-bundles
    #mdls -name kMDItemUserTags -raw "$f" -nullMarker '' | awk 'NR>1' | sed '$d' | prefixer --trim -i $',\n' --skip-empty
    ##
    command tag --garrulous --list "$f" | gsed 1d | trimsed
    ##
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

    local f
    for f in "${@:-.}" ; do
        test -e "$f" || {
            ecerr "$0: Nonexistent path: $f"
            continue
        }
        f="$(realpath "$f")"
        if test -d "$f" ; then
            local dir="${f}"
            fd ${ntag_fd_opts[@]} --type file --type symlink --type socket . "$dir" | inargsf re "$engine_q"
            fd ${ntag_fd_opts[@]} --type directory . "$dir" | inargsf re "$engine_q" # @todo @BUG: Having nested tagged directories can invalidate the path of the deeper dir. Crude workaround: run this function multiple times. (Sorting by, e.g., length should solve this?)
            reval "$engine[@]" "$dir" # last because renaming the root dir will invalidate all further operations
        else
            reval "$engine[@]" "$f"
        fi
    done
}
##
function ntag-toapple-force() {
    tag-apple-rm-colors-rec "$@"
    ntag-toapple-rec "$@"
}
function ntag-fromapple-force() {
    ntag-rm-colors-rec "$@"
    ntag-fromapple-rec "$@"
}
#### fuzzy
###
## Non-coloring, potentially fuzzy (Using `tgs`, `tgsor` instead is recommended.)
function ntag-search() {
    local orMode="${ntag_search_or}"
    local query_sep=''
    test -n "$orMode" && query_sep=' | '
    local query="$(mg_sep=' ' mapg "\'\${ntag_sep}\$i\${ntag_sep}\${query_sep}" "$@")"

    ##
    # local nightNotes="${ntag_search_dir:-.}"
    # ntsearch_glob='' ntsearch_rg_opts=(-uuu) ntl-fzf "$query"
    ##
    local input
    {
        # @weird: Increase pcat's timeout if you see weird behavior
        if input="$(pcat 10)" ; then
            ecn $input
        else
            fd ${ntag_fd_opts[@]}
        fi
    } | fzp "$query"
}
aliasfn ntag-searchor @opts or y @ ntag-search
##
aliasfn ntag-grep fnswap isI false ntag-search
@opts-setprefix ntag-grep ntag-search

aliasfn ntag-grepor fnswap isI false ntag-searchor
###
function ntag_filter_rg() {
    local pattern="$1" bg="${2:-0,0,0}" fg="${3:-255,255,255}"

    # We need to limit highlighting ntag_sep, as the color codes will impede further matches
    # `nobold`
    command rg --passthrough --smart-case --colors "match:none" --colors "match:style:bold" --fixed-strings --color always --colors "match:bg:$bg" --colors "match:fg:$fg" "${ntag_sep[-1]}${pattern}${ntag_sep[1]}"  #"${ntag_sep}${pattern}${ntag_sep}"
    return 0
}
aliasfn ntag-color ntagcolor
function ntag-color1() {
    # INPUT: stdin
    ## @perf
    # `ll --color always | time (ntag-color)` 115ms
    ##
    # https://www.regular-expressions.info/lookaround.html
    # Lookbehind needs to be fixed length in rg's pcre.
    # Use `cat -v` to see the ANSI codes. `\e` is `^[[`.
    # Hardcoded for ntag_sep=..
    ###
    # local re_def='(?<!;\dm)(?<!;\d\dm)(?<!;\d\d\dm)\.(?!\e\[)[^./]+\.(?=(?:\e(?:\e|\d|\[|;|m)*)?\.)'
    ##
    # local re_def='(?<=\.\e\[0m)(?<!;\dm)(?<!;\d\dm)(?<!;\d\d\dm)\.(?!\e\[)[^./]+\.(?=(?:\e(?:\e|\d|\[|;|m)*)?\.)'
    # local re_def2='(?<=\.)(?<!;\dm)(?<!;\d\dm)(?<!;\d\d\dm)\.(?!\e\[)[^./]+\.(?=(?:\e(?:\e|\d|\[|;|m)*)?\.)'
    ##
    # We have to use two patterns because various-length look-behind is not supported.
    local re_def='(?<=\.\e\[0m)\.(?!\e\[)[^./]+\.(?=(?:\e(?:\e|\d|\[|;|m)*)?\.)'
    local re_def2='(?<=\.)\.(?!\e\[)[^./]+\.(?=(?:\e(?:\e|\d|\[|;|m)*)?\.)'
    ##
    ntag_filter_rg blue 0,0,255  | ntag_filter_rg green 0,255,0 0,0,0 | ntag_filter_rg red 255,0,0 | ntag_filter_rg orange 255,120,0 | ntag_filter_rg yellow 255,255,0 0,0,0 | ntag_filter_rg purple 100,10,255 | ntag_filter_rg gray 100,100,100 | ntag_filter_rg grey 100,100,100 | ntag_filter_rg black 0,0,0 | ntag_filter_rg aqua 0,255,255 0,0,0 | ntag_filter_rg teal 0,128,128 | command rg --passthrough --smart-case --colors "match:none" --colors "match:style:bold" --color always --colors "match:bg:255,255,255" --colors "match:fg:255,120,0" --pcre2 -e "$re_def" -e "$re_def2"
    return 0
}
function ntag-filter() {
    : "Alt: Use ntag-grep if you never want the coloring."
    ## perf
    # `time (@opts or yes @ green red)` ~ 120ms
    # `time (fnswap isI false @opts or yes @ green red)` ~ 190ms
    ##
    local colorMode="${ntag_search_color:-${ntag_search_c}}"

    local res
    res="$(ntag-grep "$@")" || return 1
    if test -n "$colorMode" || { isI && istty } ; then
        ec $res | ntag-color
    else
        ec $res
    fi
}
@opts-setprefix ntag-filter ntag-search
aliasfn tgf ntag-filter
aliasfn ntag-filteror @opts or y @ ntag-filter
aliasfn tgfor ntag-filteror
## interactive
function ntag-filterori() {
    @opts color y or y @ ntag-filter "$@" | fz-ntag
}
aliasfn tgsor ntag-filterori
function ntag-filteri() {
    @opts color y @ ntag-filter "$@" | fz-ntag
}
aliasfn tgs ntag-filteri
###
function ntag-gethead() {
    local orig="$1"
    local orighead="$(<<<${orig:r} prefixer -i .. | ghead -n1)"
    ec "${orighead}"
}
function ntag-recoverpath() {
    local orig="$1"
    if test -e "$orig" ; then
        ec $orig
    else
        local orighead="$(ntag-gethead "$orig")"
        local candidates=( "${orighead}"..*.."${orig:e}"(DN) "${orighead}$(prefix-if-ne . "${orig:e}")" )
        local res="$candidates[1]"
        if test -e "$res" ; then
            ec $res
        else
            ec $orig
            return 1
        fi
    fi
}
##
function fz-ntag() {
    fz --ansi --preview "$FZF_PREVIEW_NTAG" --keep-right "$@"
}
##
function ntag-select() {
    local query="${1}"

    arrN $ntag_colors[@] | fzp "$query" # sponge # zsh bugs again
    ec $query
}
function ntag-finder-sel-add() {
   finder-selection-get.as | inargsf @opts add [ "$@" ] @ ntag-rmadd
}
function ntag-finder-sel-rm() {
   finder-selection-get.as | inargsf @opts rm [ "$@" ] @ ntag-rmadd
}
##
function ntag-color-dotless() {
    # @incomplete You need to add the other colors
    local FORCE_INTERACTIVE=y FORCE_NONINTERACTIVE=""
    prefixer replace -- \
        red "$(colorfg 255 0 0)red$(colorreset)" \
        orange "$(colorfg 255 120 0)orange$(colorreset)" \
        yellow "$(colorfg 255 255 0)yellow$(colorreset)"
}
function ntag-color0() {
    # @deprecated This needs to perform the line loop in zsh which kills performance. I wrote the go script `ntagcolor.go` just for this.
    local in="$(cat)"

    local FORCE_INTERACTIVE=y FORCE_NONINTERACTIVE=""
    local cmd="$(gq prefixer replace --skip-empty -i .. -o '' --replace "$(colorbg 255 255 255 ; colorfg 255 120 0 ; Bold).\$1.${reset_color}" -- \
        blue "$(colorbg 0 0 255 ; colorfg 255 255 255 ; Bold).blue.${reset_color}" \
        green "$(colorbg 0 255 0 ; colorfg 0 0 0 ; Bold).green.${reset_color}" \
        red "$(colorbg 255 0 0 ; colorfg 255 255 255 ; Bold).red.${reset_color}" \
        orange "$(colorbg  255 120 0; colorfg 255 255 255 ; Bold).orange.${reset_color}" \
        yellow "$(colorbg 255 255 0 ; colorfg 0 0 0 ; Bold).yellow.${reset_color}" \
        purple "$(colorbg 100 10 255 ; colorfg 255 255 255 ; Bold).purple.${reset_color}" \
        gray "$(colorbg 100 100 100 ; colorfg 255 255 255 ; Bold).gray.${reset_color}" \
        grey "$(colorbg 100 100 100 ; colorfg 255 255 255 ; Bold).grey.${reset_color}" \
        black "$(colorbg 0 0 0 ; colorfg 255 255 255 ; Bold).black.${reset_color}" \
        aqua "$(colorbg 0 255 255 ; colorfg 0 0 0 ; Bold).aqua.${reset_color}" \
        teal "$(colorbg 0 128 128 ; colorfg 255 255 255 ; Bold).teal.${reset_color}")"

    local i
    for i in ${(@f)in} ; do
        if [[ "$i" =~ '([^.]*)\.\.(.*)\.\.([^.]*)' ]] ; then
            local colored="$(ecn "$match[2]" | eval "$cmd" )"

            ec "${match[1]}.${colored}.${match[3]}"
        else
            ec "$i"
        fi
    done
}
##
function air-blue() {
    ntag-add "$(hear-get)" blue
}
