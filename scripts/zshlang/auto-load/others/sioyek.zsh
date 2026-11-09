##
function sioyek-reload {
    #: =reload_no_flicker= exists only in our fork; upstream has =reload=.
    #:
    #: Without =--nofocus=, the running instance calls =raise()= on its window,
    #: which Qt implements on macOS as =[NSApp activateIgnoringOtherApps:YES]=,
    #: so every reload steals focus. That is unbearable when an agent
    #: recompiles a deck dozens of times. =--nofocus= is upstream since 2022
    #: and present in the stock binary, too.
    #:
    #: Set =sioyek_reload_focus_p=y= to let the reload raise the window. It
    #: controls only sioyek's own raise; it does not call [agfi:sioyek-focus].
    ##
    local focus_p="${sioyek_reload_focus_p:-n}"

    local opts=()
    if ! bool "${focus_p}" ; then
        opts+=(--nofocus)
    fi

    awaysh sioyek --execute-command reload_no_flicker "${opts[@]}"
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

        #: The databases live in =~/Library/Application Support/sioyek/=; sioyek
        #: on macOS never reads =*.db= next to its binary. Do not copy any into
        #: the bundle: a file added after signing breaks the signature's seal.
        #: (An old layout kept them there; that backup is in =~base/_Local TMP/sioyek_db=.)

        trs /Applications/sioyek.app
        command gcp -v -r build/sioyek.app /Applications/
        #: A certificate signature identifies the app by bundle id and
        #: certificate, so macOS privacy grants (e.g., folder access) survive
        #: rebuilds; an ad hoc one is the hash of this exact build. Set up once
        #: per Mac, at its screen: =macos-local-signing ensure 'Sioyek Local Signing'=
        #: https://github.com/NightMachinery/macos-local-signing
        #: No =sudo=: the bundle we just copied is ours.
        local signing_identity="${sioyek_signing_identity:-Sioyek Local Signing}"
        if (( ${+commands[macos-local-signing]} )) &&
            macos-local-signing ready "${signing_identity}" ; then
            ecgray "signing as '${signing_identity}'"
            macos-local-signing codesign "${signing_identity}" --force --deep /Applications/sioyek.app
        else
            ecgray "signing ad hoc (run once: macos-local-signing ensure '${signing_identity}')"
            command codesign --force --deep --sign - /Applications/sioyek.app
        fi

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
