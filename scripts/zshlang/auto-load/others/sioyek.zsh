##
function sioyek-reload {
        # sioyek --execute-command reload
        sioyek --execute-command reload_no_flicker
}
##
function build-sioyek {
    reset ; bella_zsh_disable1 ; (
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
