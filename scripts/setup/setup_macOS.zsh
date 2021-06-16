#!/usr/bin/env zsh

if isDarwin ; then
    objc-compile "$NIGHTDIR/objective-c/input_lang_get_objc.m"

    brew install --cask --appdir=/Applications megacmd

    brew install fabianishere/personal/pam_reattach

    brew install --cask corelocationcli
    ## Fonts:
    brew install --cask font-fira-code font-fira-mono
    brew install --cask font-victor-mono
    brew install --cask font-vazir-code
    brew install --cask font-iosevka
    brew install --cask font-juliamono
    ##
    git clone https://github.com/chbrown/macos-pasteboard
    pushf macos-pasteboard
    {
        make install
    } always {
        popf
    }
    ##
fi
