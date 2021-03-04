rm-deleteus() {
    ! test -e "$deleteus" && return 0
    re trs "${(@f)$(<"$deleteus")}"
    mv "$deleteus" "$deleteus.pbak"
}
##
function rm-caches-sudo() {
    if isLinux ; then
        sudo apt-get clean
        sudo apt-get autoclean
        sudo apt-get autoremove -y
        # sudo rm -rf /var/log/* # this can break things that depend on some directories existing. Delete manually via ncdu.
    fi
}
function rm-caches() {
    rm-deleteus
    silent command rm -r ~/.local/share/Trash/*(DN)
    silent command rm ~/sent
    command rm -rf "$(brew --cache)"
    conda clean --all --yes # STILL does not delete old python version stuff!
    trs-empty
}
function rm-caches-all() {
    rm-caches-sudo
    rm-caches
}
##
