###
function play-tag() {
    local f="$1"
    # tag --add green "$f"
    ntag-add "$f" green
    rgeval mpv "$(realpath "$ntag_add_dest")" # realpath to unbreak rgeval's usefulness because we use this via `indir`.
}
function openv() {
    # om sorts by moddate
    {
        local vids="$(arrN **/*.(${(j.|.)~video_formats})(.DNom))"
        <<<$vids ntag-grepor green # to have these at first
        <<<$vids ntag-grepor gray grey
        ec $vids
    } | ntag-color | fz --ansi | inargsf play-tag
}
function delenda() {
    ntag-filterori red green gray grey | inargsf trs
}
##
zv() { indir "$*" openv }
aliasfn r2 incache openv
lec() { indir ~/base/Lectures openv }
###
