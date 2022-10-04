##
aliasfnq bat command bat --theme OneHalfLight --pager="less -FRXn"
aliasfn bat-m bat --style=plain
aliasfn bt bat-m
aliasfn btz bat-m --language zsh

aliasfn bat-header bat --decorations=always --style=header

function bat-py {
    cat-paste-if-tty |
        bat-m --language python
}
aliasfn btp bat-py
##
