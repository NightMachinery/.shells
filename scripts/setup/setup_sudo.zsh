#!/usr/bin/env zsh

##
function mosh-ins {
    if isLinux ; then
        sudo apt remove mosh || true
        # use brew to install the HEAD:
        brew remove mosh ; brew install mosh --head
    fi
    sudo rm -f /usr/local/bin/mosh-server
    rehash
    reval-ec sudo ln -s "${commands[mosh-server]}" /usr/local/bin/
}
##
