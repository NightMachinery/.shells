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
function rsp-notes-export {
    local rsp_include
    rsp_include=(${(@f)"$(org-export-recursive "${@}" | trim-extension)"}) @TRET

    if (( ${#rsp_include} == 0 )) ; then
        ecerr "$0: no files included"
        return 1
    fi

    ec $'\n\n#####################\n\n'

    reval-ec rsp-notes ~nt/ --rsh="ssh -J walle@193.151.136.67"
}

function rsp-notes {
    bella_zsh_disable1

    local src="$1"
    src="$(realpath "$src")" @TRET

    local rsync_opts=("${@[2,-1]}")

    local dest="${rsp_dest:-${lilf_user}@${lilf_ip}:Downloads/static}"
    local pattern="${rsp_pat:-.}"
    assert-args src dest pattern @RET

    ensure-array rsp_include
    local includes=(${(@)rsp_include})
    local includes_cmd=()
    if ((${#includes} >= 1)) ; then
        includes_cmd=(ugrep -F)
        local i
        for i in ${includes[@]} ; do
            includes_cmd+=(-e "${i}")
        done
    else
        includes_cmd=(cat)
    fi

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

    {
    #: @warning These paths seem to be relative to all included directories. So having 'notes' will cause 'x/notes' to also be included. But I think this will at most cause some empty directories to be copied in our situation, and not files.
    reval-ec fd -uuu --type=file --full-path \
        "${opts[@]}" \
        "$pattern" \
        "$src" |
        "${includes_cmd[@]}" |
        prefixer -r "$starting_dir" |
        prefixer -r "/" |
        {
            ec "$src"
            path-parent-dirs
        } |
        duplicates-clean |
        reval-ec rsp-safe --include-from=- --exclude='*' \
            "${rsync_opts[@]}" \
            "$src" "$dest"
    } always { bell-hp3-star-pickup }
}
@opts-setprefix rsp-notes rsp
##
