##
function surreal-sync2remote {
    local fullhost=pinky

    reval-ec rsync --verbose --compress --safe-links --links --archive --checksum --protect-args --human-readable --xattrs --times --info=progress2 --partial-dir=.rsync-partial -r --exclude='.git' --max-delete=100 --fuzzy --delete-after --force-delete -- ~/Pictures/SurrealPictures ${fullhost}:Pictures/
}
##
