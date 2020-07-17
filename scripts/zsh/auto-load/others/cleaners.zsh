rm-deleteus() {
    ! test -e "$deleteus" && return 0
    re trs "${(@f)$(<"$deleteus")}"
    mv "$deleteus" "$deleteus.pbak"
}

function rm-caches() {
    sudo apt-get clean
    sudo apt-get autoclean
    sudo apt-get autoremove -y
    sudo rm -rf /var/log/*
    rm ~/sent
    rm -rf "$(brew --cache)"
}
