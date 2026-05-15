##
function surreal-sync2remote {
    local fullhost=pinky

    reval-ec rsync \
        --verbose --compress \
        --safe-links --links --archive --checksum \
        --protect-args --human-readable --xattrs --times \
        --info=progress2 \
        --partial-dir=.rsync-partial \
        --exclude='.git' \
        --fuzzy \
        --delete-after \
        --delete-excluded \
        --force-delete \
        --include='*/' \
        --include='**/chosen*/***' \
        --exclude='*' \
        -- ~/Pictures/SurrealPictures/ \
        "${fullhost}:Pictures/SurrealPictures/"

    # --max-delete=100 \
    # @GPT5.5T
    # Rsync filters are ordered; first matching rule wins.
    # Include all dirs (`*/`) so rsync can recurse through parents and find nested chosen*/ dirs.
    # `*/` applies at every depth, e.g. a/, a/b/, a/b/c/.
    # `**/chosen*/***` includes any dir named chosen* at any depth, plus all contents below it.
    # `***` is rsync-specific shorthand for "the dir itself and everything recursively under it".
    # Final `--exclude='*'` excludes everything not previously included.
    # `--delete-excluded` also removes excluded destination files, not just files missing from source.
    # `--max-delete=100` is a safety brake: abort if more than 100 deletes would happen.
    # Test destructive filter changes with `--dry-run --itemize-changes`.
}

function surreal-sync2remote-v1 {
    local fullhost=pinky

    reval-ec rsync --verbose --compress --safe-links --links --archive --checksum --protect-args --human-readable --xattrs --times --info=progress2 --partial-dir=.rsync-partial -r --exclude='.git' --max-delete=100 --fuzzy --delete-after --force-delete -- ~/Pictures/SurrealPictures ${fullhost}:Pictures/
}
##
