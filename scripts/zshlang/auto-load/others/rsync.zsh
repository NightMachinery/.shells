aliasfn rsp-dl rsync --protect-args --human-readable --xattrs --times --info=progress2 --append -r # append assumes files only grow and do not otherwise change

aliasfn rsp-safe rsync --verbose --checksum --protect-args --human-readable --xattrs --times --info=progress2 --partial-dir=.rsync-partial -r # partial-dir supports resume
#: @toFuture/1404 add =--mkpath=, currently most installed rsync are too old to support it

aliasfn rsp rsp-safe --delete-after --force-delete #--ignore-errors will delete even if there are IO errors on sender's side.
aliasfn rspm rsp --crtimes
aliasfn rspb rsp --backup --backup-dir=.rsync-backup
aliasfn rspbm rspb --crtimes
##
function path-parent-dirs {
    awk '{print; while(/\//) {sub("/[^/]*$", ""); print}}'
}
##
function lilf-link-notes {
    local inargs
    in-or-args3 "$@" @RET

    local i i_html
    for i in ${inargs[@]} ; do
        i_html="${i:r}.html"
        if [[ "$i" == *.org ]] && test -e "${i_html}" ; then
            i="$i_html"
        fi

        ec "${dl_base_url}/static/notes/$(grealpath --relative-to "$nightNotes" "$i")"  | url-encode
        #: url-encode encodes newlines, so we need to apply it per link.
    done | cat-copy-if-tty
}

function rsp-notes-export {
    local rsp_include entries=("${@}")
    rsp_include=(${(@f)"$(org-export-recursive "${entries[@]}" | trim-extension)"}) @TRET

    if (( ${#rsp_include} == 0 )) ; then
        ##
        # rsp_include=("${entries[@]}")
        #: This wouldn't move the images.
        #: Just edit the files for now to make their old stored hashes invalid.
        ##
        ecerr "$0: no files included"
        return 1
        ##
    fi

    ec $'\n\n#####################\n\n'
    reval-ec rsp-notes ~nt/ @RET

    ec $'\n\n#####################\n\n'
    lilf-link-notes "${entries[@]}"
}

function rsp-notes {
    bella_zsh_disable1

    local src="$1"
    src="$(realpath "$src")" @TRET

    local rsync_opts=(
        ## @tmp
        # --rsh="ssh -p 4300 -o ControlMaster=no -vvv"
        --rsh="ssh -J afshan -o ControlMaster=no -vvv"
        # --rsh="ssh -J walle@193.151.136.67"
        ##
        "${@[2,-1]}"
    )

    # local lilf_ip=193.151.136.67 #: @tmp
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

    local exts=( ${image_formats[@]} html css ) #: @Warning Do NOT add org to this list, as then our private :noexport: subtrees will be exposed!
    local ext
    for ext in ${exts[@]} ; do
        opts+=(--extension "$ext")
    done

    {
    #: @warning These paths seem to be relative to all included directories. So having 'notes' will cause 'x/notes' to also be included. But I think this will at most cause some empty directories to be copied in our situation, and not files.
    reval-ec fd -uuu --type=file --full-path \
        "${opts[@]}" \
        "$pattern" \
        "$src" |
        revaldbg "${includes_cmd[@]}" |
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
