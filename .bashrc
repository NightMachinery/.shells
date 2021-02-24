### ZSH COMPATIBLE
##
export NIGHTDIR=~/scripts
source "$NIGHTDIR"/zsh/basic/conditions.zsh
##
if isBash ; then
    if (( BASH_VERSINFO[0] >= 5 )) ; then
        shopt -s globstar
    fi
fi
function run-on-each() {
    local i98765
    for i98765 in "${@:2}"
    do
        "$1" "$i98765"
    done
}
alias re=run-on-each
function addToPATH {
    export PATH="$1:$PATH"
}
##
source ~/.shared.sh
### Interactive
if isI ; then
    bind '"\e[A": history-search-backward'
    bind '"\e[B": history-search-forward'
    ##
    # This takes a whole second to load
    # [ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion
    ##
fi
##
BASHRC_LOADED=y

test -e "$HOME/.shellfishrc" && source "$HOME/.shellfishrc"
