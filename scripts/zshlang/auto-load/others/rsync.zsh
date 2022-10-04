aliasfn rsp-dl rsync --protect-args --human-readable --xattrs --times --info=progress2 --append -r # append assumes files only grow and do not otherwise change
aliasfn rsp-safe rsync --verbose --checksum --protect-args --human-readable --xattrs --times --info=progress2 --partial-dir=.rsync-partial -r # partial-dir supports resume
aliasfn rsp rsp-safe --delete-after --force-delete #--ignore-errors will delete even if there are IO errors on sender's side.
aliasfn rspm rsp --crtimes
aliasfn rspb rsp --backup --backup-dir=.rsync-backup
aliasfn rspbm rspb --crtimes
##
function path-parent-dirs {
    awk '{print; while(/\//) {sub("/[^/]*$", ""); print}}'
}
##
function rsp-notes {
    local src="$1"
    src="$(realpath "$src")" @TRET

    local rsync_opts=("${@[2,-1]}")

    local dest="${rsp_dest:-${lilf_user}@${lilf_ip}:Downloads/static}"
    local pattern="${rsp_pat:-.}"
    assert-args src dest pattern @RET

    local starting_dir="$src"
    if [[ "$src" != */ ]] ; then
        starting_dir="${src:h}"
        #: The src dir itself will be copied, so the starting dir is its parent.
    fi

    local opts=()

    local ext exts=( ${image_formats[@]} html css )
    for ext in ${exts[@]} ; do
        opts+=(--extension "$ext")
    done

    reval-ec fd -uuu --type=file --full-path \
        "${opts[@]}" \
        "$pattern" \
        "$src" |
        prefixer -r "$starting_dir" |
        prefixer -r "/" |
        {
            ec "$src"
            path-parent-dirs
        } |
        reval-ec rsp-safe --include-from=- --exclude='*' \
            "${rsync_opts[@]}" \
            "$src" "$dest"
}
@opts-setprefix rsp-notes rsp
##
