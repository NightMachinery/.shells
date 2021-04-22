#!/usr/bin/env zsh

##
if isLinux ; then
    sudo apt remove mosh || true
    # use brew to install the HEAD: `brm mosh ; bi mosh --head`
fi
sudo rm -f /usr/local/bin/mosh-server
rehash
reval-ec sudo ln -s "${commands[mosh-server]}" /usr/local/bin/
##
