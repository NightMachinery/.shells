###
function play-tag() {
    local f="$1"
    # tag --add green "$f"
    ntag-add "$f" green
    rgeval mpv "$ntag_add_dest"
}
function openv() {
    # om sorts by moddate
    {
        local vids="$(arrN **/*.(${(j.|.)~video_formats})(.DNom))"
        <<<$vids green # to have these at first
        ec $vids
    } | ntag-color | fz --ansi | inargsf play-tag
}
##
zv() { indir "$*" openv }
aliasfn r2 incache openv
lec() { indir ~/base/Lectures openv }
###
