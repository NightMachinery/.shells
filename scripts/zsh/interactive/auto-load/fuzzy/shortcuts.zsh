###
function play-tag() {
    local f="$1"
    # tag --add green "$f"
    ntag-add "$f" green
    rgeval mpv "$(realpath "$ntag_add_dest")" # realpath to unbreak rgeval's usefulness because we use this via `indir`.
}
function openv() {
    local dirs=(${openv_dirs[@]}) query="$(fz-createquery "$@")"
    test -z "$dirs[*]" && dirs=(.)

    local i vids=''
    {
        # for i in "$dirs[@]" ; do
        #     # om sorts by moddate
        #     vids+="$(arrN $i/**/*.(${(j.|.)~video_formats})(.DNom))"
        # done

        # fd has nondeterministic sort order.
        vids="$(fd -uuu --type file --regex "\.(${(j.|.)video_formats})\$" "$dirs[@]")"
        dvar vids
        <<<$vids ntag-grepor green # to have these at first
        <<<$vids ntag-grepor gray grey
        ec $vids
    } | ntag-color | fz --ansi --query "$query" | inargsf play-tag
}
function delenda() {
    ntag-filterori red green gray grey | inargsf trs
}
##
zv() {
    local q="$*"

    if test -n "$q" ; then
        indir "$q" openv
    else
        @opts dirs [ ~/base/cache ~/base/Lectures ~/base/series ~/base/anime ~/"base/_Local TMP" ~/base/docu ~/base/movies ~/base/V ~/base/dls ~/Downloads ] @ openv
    fi
}
aliasfn r2 incache openv
lec() { indir ~/base/Lectures openv "$@" }
###
