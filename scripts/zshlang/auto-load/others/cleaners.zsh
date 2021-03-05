rm-deleteus() {
    ! test -e "$deleteus" && return 0
    re trs "${(@f)$(<"$deleteus")}"
    mv "$deleteus" "$deleteus.pbak"
}
##
function rm-caches-sudo() {
    if isLinux && ! isBorg ; then
        sudo apt-get clean
        sudo apt-get autoclean
        sudo apt-get autoremove -y
        # sudo rm -rf /var/log/* # this can break things that depend on some directories existing. Delete manually via ncdu.
    fi
}
function trs-rm() {
    local paths=("$@")

    local p
    for p in $paths[@] ; do
        if test -e "$p" ; then
            reval-ec command rm -rf "$p"
        fi
    done
}
function rm-caches() {
    rm-deleteus
    trs-rm  ~/.local/share/Trash/*(DN)
    trs-rm ~/sent
    trs-rm ~/logs ~/log
    trs-rm ~/julia_tmp/ ~/tmp-kindle
    trs-rm "$(brew --cache)"
    conda clean --all --yes # STILL does not delete old python version stuff!
    trs-empty
}
function rm-caches-all() {
    rm-caches-sudo
    rm-caches
}
aliasfn cleanup rm-caches-all
##
