#!/usr/bin/env zsh

##
if isLinux ; then
    sudo apt remove mosh || true
    # use brew to install the HEAD: `brm mosh ; bi mosh --head`
fi
sudoify ln reval-ec lnrp "${commands[mosh-server]}" /usr/local/bin/
##
