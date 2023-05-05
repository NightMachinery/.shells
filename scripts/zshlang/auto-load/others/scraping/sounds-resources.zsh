function sounds-resources-dl {
    local urls=("$@") # e.g., https://www.sounds-resource.com/pc_computer/starcraftiiheartoftheswarm/

    local noconfirm
    dir-isempty && noconfirm=y

    # @warn sounds-resource can have different files with the same name
    { eval-memoi getlinks-c -e '/sound/' "$urls[@]"  | gsort -u | memoi_key="$urls[*]" reval-true eval-memoi @opts halt never @ para getlinks-c -e '/download/' | gsort -u | prefixer --skip-empty | @opts jobs 6 @ para -v aa_rename=y aa-remotename } || { retcode ; return $? }
      # @opts jobs 6 @ para -v curl-dl --retry 10 --retry-delay 1 }
      # inargsf curl-dl -Z

    if test -n "$noconfirm" || ask "Unzip and delete all .zip files in '$PWD'? (WARNING: There were other files present before ${0})" ; then
        para -k unzip2dir ::: *.zip @RET
        para trs ::: *.zip
    fi
}
##
function gc-tag() {
    : "@alt lkh (ntag-ls-head)"

    local f="${1:-$(hear-get)}"

    trapexits
    ( heari "$f" )
    trapexits-release

    if ask "Proceed with $(gq "$f")?" Y ; then
        local tag=''
        vared -p "tag: " tag || tag=''
        ntag-add "$f" $tag blue @RET
        vared -p "bell-maker: " tag || tag=''
        if test -n "$tag" ; then
            reval-copy gquote bell-maker "${tag}" "$(grealpath "${ntag_add_dest}" | dir-rmprefix "$(grealpath "$GREENCASE_DIR")")"
        fi
    fi
}
alias hear-current-save-as-bell='gc-tag'
##
