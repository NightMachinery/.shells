##
#: `-r` is `--recursive` like `cp -r`

aliasfn rsp-dl rsync --protect-args --human-readable --xattrs --times --info=progress2 --append -r # append assumes files only grow and do not otherwise change

aliasfn rsp-safe rsync --verbose --checksum --protect-args --human-readable --xattrs --times --info=progress2 --partial-dir=.rsync-partial -r --mkpath # partial-dir supports resume
#: @toFuture/1404 add =--mkpath=, currently most installed rsync are too old to support it

aliasfn rsp-safe2 enh_dest_shift_e=(rsp-safe) enh-dest-shift

aliasfn rsp rsp-safe --delete-after --force-delete # --ignore-errors will delete even if there are IO errors on sender's side.
aliasfn rspm rsp --crtimes
aliasfn rspb rsp --backup --backup-dir=.rsync-backup
aliasfn rspbm rspb --crtimes
##
function path-parent-dirs {
    awk '{print; while(/\//) {sub("/[^/]*$", ""); print}}'
}
##
function static-public-ls {
    fd -t f . ~dl/static/public
}
##
function lilf-link-notes {
    local inargs
    in-or-args3 "$@" @RET

    local public_p="${rsp_notes_public_p:-n}"

    local dir
    if bool "${public_p}" ; then
        dir='static/public/notes'
    else
        dir='static/notes'
    fi

    local i i_html
    for i in ${inargs[@]} ; do
        i_html="${i:r}.html"
        if [[ "$i" == *.org ]] && test -e "${i_html}" ; then
            i="$i_html"
        fi

        ec "${dl_base_url}/${dir}/$(grealpath --relative-to "$nightNotes" "$i")"  | url-encode
        #: url-encode encodes newlines, so we need to apply it per link.
    done | cat-copy-if-tty
}

function rsp-notes-export {
    local rsp_include entries=("${@}")
    rsp_include=(${(@f)"$(org_export_root_dir="${nightNotes}" org-export-recursive "${entries[@]}")"}) @TRET

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
    src="$(grealpath -- "$src")" @TRET
    local public_p="${rsp_notes_public_p:-n}"

    local rsync_opts=(
        ## @tmp
        # --rsh="ssh -p 4300 -o ControlMaster=no -vvv"
        # --rsh="ssh -J afshan -o ControlMaster=no -vvv"
        # --rsh="ssh -J walle@193.151.136.67"
        ##
        "${@[2,-1]}"
    )

    # local lilf_ip=193.151.136.67 #: @tmp
    local dest
    dest="${rsp_notes_dest:-${lilf_user}@${lilf_ip}:Downloads/static}"
    if bool "${public_p}" ; then
        dest+='/public'
    fi

    local pattern="${rsp_pat:-.}"
    assert-args src dest pattern @RET

    ensure-array rsp_include
    local includes=(${(@)rsp_include})
    local includes_cmd=()
    if ((${#includes} >= 1)) ; then
        if true ; then
            local pattern_file
            pattern_file="$(mktemp)" @TRET

            if true ; then
                arrnn ${includes[@]} > "${pattern_file}"
                #: [[id:0e2bf436-2574-4169-8b21-722cf0b05d1c][Literal escape sequences do NOT work when read from a file]]
            fi

            # dact var-show pattern_file
            includes_cmd=(perl -nle 'BEGIN { open my $fh, "<", shift or die $!; @patterns = <$fh>; chomp @patterns; close $fh } foreach my $pattern (@patterns) { if (/^\Q$pattern\E$/) { print; last } }' "$pattern_file")
        else
            #: @deprecated
            ##
            includes_cmd=(ugrep -F)
            local i
            for i in ${includes[@]} ; do
                i="${i:r}" #: trim extension
                includes_cmd+=(-e "${i}")
                #: This will NOT add `^` or `$`, so if we have included `/a.org`, `/a.org_attachments/...` will also match.
                #: We are also trimming the extensions, so `/a.js` will also match.
            done
        fi
    else
        includes_cmd=(cat)
    fi

    local starting_dir="$src"
    if [[ "$src" != */ ]] ; then
        starting_dir="${src:h}"
        #: The src dir itself will be copied, so the starting dir is its parent.
    fi

    local opts=()

    local exts=(
        ${image_formats[@]}
        html
        css
    ) #: @Warning Do NOT add org to this list, as then our private :noexport: subtrees will be exposed!
    local ext
    for ext in ${exts[@]} ; do
        opts+=(--extension "$ext")
    done

    # dact var-show src
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
