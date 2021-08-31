#!/usr/bin/env zsh

if isDarwin ; then
    objc-compile "$NIGHTDIR/objective-c/input_lang_get_objc.m"

    brew install --cask --appdir=/Applications megacmd

    brew install fabianishere/personal/pam_reattach

    brew install --cask corelocationcli

    brew install --cask rar # unrar

    # credential manager for HTTPS logins
    # https://docs.github.com/en/get-started/getting-started-with-git/caching-your-github-credentials-in-git
    brew tap microsoft/git
    brew install --cask git-credential-manager-core
    ## Fonts:
    brew install --cask font-fira-code font-fira-mono
    brew install --cask font-victor-mono
    brew install --cask font-vazir-code
    brew install --cask font-iosevka
    brew install --cask font-juliamono
    ##
    # installing pbv:
    git clone https://github.com/chbrown/macos-pasteboard
    pushf macos-pasteboard
    {
        make install
    } always {
        popf
    }
    ##
fi
