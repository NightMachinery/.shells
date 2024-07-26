##
function sioyek-reload {
    # sioyek --execute-command reload
    sioyek --execute-command reload_no_flicker
}

function sioyek-focus {
    if isDarwin ; then
        focus-app 'info.sioyek.sioyek'
    else
        @NA
    fi
}
##
function build-sioyek {
    reset ; bella_zsh_disable1
    (
        setopt PIPE_FAIL PRINT_EXIT_VALUE ERR_RETURN

        export SIOYEK_BUILD_INCREMENTAL_P=y
        export SIOYEK_NIGHT_P=y

        # z sioyek

        MAKE_PARALLEL=16 bash ./build_mac.sh

        unsetopt SOURCE_TRACE XTRACE

        local d='/Applications/sioyek.app/Contents/MacOS/'
        local backup_d=~base'/_Local TMP/sioyek_db'

        cp2 "${backup_d}"/ "$d"/*.db(DN)

        trs /Applications/sioyek.app
        command gcp -v -r build/sioyek.app /Applications/
        sudo codesign --force --sign - --deep /Applications/sioyek.app

        cp2 "$d" "${backup_d}"/*(DN)

        bell-p2-target-acquired
    ) ; (( $? == 0 )) || bell-fail
}
##
function h-sioyek-org-pdf-link-create {
    local file_path="$1" page_number="$2" offset_x="$3" offset_y="$4" zoom_level="$5"
    local focus_emacs_p="y"
    assert-args file_path page_number offset_x offset_y zoom_level @RET

    file_path="$(path-abbrev "${file_path}")" @TRET

    page_number=$(( page_number + 1 ))
    #: sioyek returns zero-based, but expects one-based page numbers.

    ecn "[[sioyek-v1:$(org-escape-link "${file_path}")::${page_number}:${offset_x}:${offset_y}:${zoom_level}]]" |
        pbcopy

    if bool "${focus_emacs_p}" ; then
        emc-focus-gui
    fi
    # bell-sc2-mousedown1
}
##
