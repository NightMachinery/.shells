###
function play-tag() {
    local f="$1"
    # tag --add green "$f"
    ntag-add "$f" green
    rgeval awaysh mpv "$(realpath "$ntag_add_dest")" # realpath to unbreak rgeval's usefulness because we use this via `indir`.
}
function ntag-lv-nocolor() {
    local fd_args=("$@") noprioMode="${ntag_lv_nopriority}"

    local vids=''

    ##
    # fd has nondeterministic sort order, and gsort takes too long.
    # perf:
    # `hyp 'fd --color always -uuu --type file --regex \\.\(ape\|avi\|flv\|mp4\|mkv\|mov\|mpeg\|mpg\|rm\|webm\)\$  ~/base/cache ~/base/Lectures ~/base/series ~/base/anime ~/"base/_Local TMP" ~/base/docu ~/base/movies ~/base/V ~/base/dls ~/Downloads  ' 'fd --color always -uuu --type file --regex \\.\(ape\|avi\|flv\|mp4\|mkv\|mov\|mpeg\|mpg\|rm\|webm\)\$  ~/base/cache ~/base/Lectures ~/base/series ~/base/anime ~/"base/_Local TMP" ~/base/docu ~/base/movies ~/base/V ~/base/dls ~/Downloads | gsort'`
    # `time2 fnswap fzf-gateway true zv`
    ##
    vids="$(fd --color always -uuu --type file --regex "\.(${(j.|.)video_formats})\$" "$fd_args[@]")"
    if test -z "$noprioMode" ; then
        # @perf1 rewrite this part using a sort function
        local prio=''
        prio="$(<<<$vids ntag-grepor green)" # to have these at first
        prio+="$(prefix-if-ne $'\n' "$(<<<$vids ntag-grepor aqua | prefixer rm -- "${(@f)prio}")" "$prio")"
        prio+="$(prefix-if-ne $'\n' "$(<<<$vids ntag-grepor teal | prefixer rm -- "${(@f)prio}")" "$prio")"
        prio+="$(prefix-if-ne $'\n' "$(<<<$vids ntag-grepor gray grey | prefixer rm -- "${(@f)prio}")" "$prio")"
        test -n "$prio" && ec $prio
        # ec $vids | prefixer rm --skip-empty -- "${(@f)prio}"
        ec $vids | prefixer rm --skip-empty -- "${(@f)prio}" | sort
        # ec $vids | prefixer rm --skip-empty -- "${(@f)prio}" | cat
    else
        ec $vids
    fi
}
function ntag-lv() {
    ntag-lv-nocolor "$@" | ntag-color
}
@opts-setprefix ntag-lv-nocolor ntag-lv
function openv() {
    local dirs=(${openv_dirs[@]}) query="$(fz-createquery "$@")"
    test -z "$dirs[*]" && dirs=(.)
    @opts nopriority '' @ ntag-lv "$dirs[@]" | fz-ntag --delimiter / --with-nth  -3..-1 --query "$query" --preview-window down:4:wrap | inargsf play-tag
}
function delenda() {
    ntag-filterori red green aqua teal gray grey | inargsf trs
}
##
zv() {
    local q="$1" ; (( $#@ >= 1 )) && shift
    local query=("$@")

    if test -n "$q" ; then
        indir "$q" openv $query[@]
    else
        @opts dirs [ ~/base/cache ~/base/Lectures ~/base/series ~/base/anime ~/"base/_Local TMP" ~/base/docu ~/base/movies ~/base/V ~/base/dls ~/Downloads ] @ openv $query[@]
    fi
}
aliasfnq zvv zv ''
aliasfn r2 incache openv
lec() { indir ~/base/Lectures openv "$@" }
###
