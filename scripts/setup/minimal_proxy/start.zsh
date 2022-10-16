#!/usr/bin/env zsh
##
sudo apt-get update -y
for pkg in $pkgs[@] ; do
    install "$pkg"
done
##
