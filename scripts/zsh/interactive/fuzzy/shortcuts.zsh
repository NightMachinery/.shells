function play-tag() {
    local f="$1"
    tag --add Green "$f"
    mpv "$f"
}
function openv() {
    arrN $~videoglob | fz | inargsf play-tag
}
aliasfn r2 incache openv
aliasfn lec indir ~/base/Lectures openv
