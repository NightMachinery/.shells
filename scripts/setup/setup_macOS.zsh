#!/usr/bin/env zsh

if isDarwin ; then
    objc-compile "$NIGHTDIR/objective-c/input_lang_get_objc.m"

    brew install util-linux
    lnrp "$(brew --prefix util-linux)/bin/setsid" ~/bin/setsid

    brew install --cask mactex
    brew install --cask handbrake
    brew install --cask adobe-acrobat-reader skim # foxit-pdf-editor

    brew unlink mpv || true
    brew install --cask mpv iina

    brew install --cask --appdir=/Applications megacmd

    brew install teamookla/speedtest/speedtest

    brew install fabianishere/personal/pam_reattach

    brew install --cask corelocationcli &&
        rehash &&
        sudo location_permission_allow_darwin.sh "${commands[CoreLocationCLI]}"

    brew install --cask rar # unrar

    # credential manager for HTTPS logins
    # https://docs.github.com/en/get-started/getting-started-with-git/caching-your-github-credentials-in-git
    brew tap microsoft/git
    brew install --cask git-credential-manager-core
    ## Fonts:
    brew tap homebrew/cask-fonts

    brew install --cask font-fira-code font-fira-mono
    brew install --cask font-victor-mono
    brew install --cask font-vazir-code
    brew install --cask font-iosevka
    brew install --cask font-juliamono
    ##
    # installing pbv:
    pushf ~cod/misc
    {
      git clone https://github.com/chbrown/macos-pasteboard
      cd macos-pasteboard
      sudo make install
    } always {
        popf
    }
    ##
    brew install pinentry-mac gpg
    sudo ln -s /opt/homebrew/bin/pinentry-mac /usr/local/bin/pinentry-mac 
    ##
    mkdir -p ~/code/misc && cd ~/code/misc && git clone https://github.com/nriley/brightness.git && cd brightness && make && sudo make install 
    #: Homebrew is not up-to-date: https://github.com/nriley/brightness/issues/43
    ##
    brew install --cask hammerspoon && ln -s /Applications/Hammerspoon.app/Contents/Frameworks/hs/hs ~/bin/ 
    ##
fi
