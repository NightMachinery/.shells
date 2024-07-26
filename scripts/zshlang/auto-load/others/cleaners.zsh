##
function rm-deleteus {
    trs-rm "$deleteusdir"
    ##
    ! test -e "$deleteus" && return 0
    trs-rm "${(@f)$(<"$deleteus")}"
    mv "$deleteus" "$deleteus.pbak"
}
##
function rm-caches-sudo {
    if isLinux && ! isBorg ; then

        sudo apt-get clean
        sudo apt-get autoclean
        sudo apt-get autoremove -y

        sudo journalctl --vacuum-time=1s
        # sudo rm -rf /var/log/* #: This can break things that depend on some directories existing. Delete manually via ncdu.
    fi
}

function trs-rm {
    local paths=("$@")

    local p
    for p in $paths[@] ; do
        if test -e "$p" ; then
            reval-ec command rm -rf -- "$p"
        fi
    done
}

function rm-caches {
    rm-deleteus
    trs-rm ~/.local/share/Trash/*(DN)
    trs-rm ~/sent
    trs-rm /tmp/*.png(DN) #: wallpaper-auto can leave garbage here, which on a long-lived server will obviously not clear on the nonexistent reboots

    trs-rm ~/logs/*(DN) ~/log/*(DN) #: do NOT delete these folders themselves

    trs-rm ~/julia_tmp/*(DN) ~/tmp-kindle/*(DN)
    trs-rm ~/tmp/hs_whisper
    trs-rm "$(brew --cache)"

    pip cache remove '*'
    conda clean --all --yes #: STILL does not delete old python version stuff!

    trs-empty
}

function rm-caches-all {
    rm-caches

    if isSudo || isI; then
        rm-caches-sudo
    else
        ecgray "$0: sudo needed for rm-caches-sudo; skipped"
    fi
}
##
function cleanup {
    reval-ec rm-caches-all
    reval-ec trs-empty
    isLocal && reval-ec clean-deps
    reval-ec brew cleanup
}
##
