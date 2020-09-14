function play-tag() {
    local f="$1"
    # tag --add green "$f"
    ntag-add "$f" green
    mpv "$ntag_add_dest"
}
function openv() {
    arrN $~videoglob | fz | inargsf play-tag
}
aliasfn r2 incache openv
aliasfn lec indir ~/base/Lectures openv
