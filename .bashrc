### ZSH COMPATIBLE
# /usr/local/bin/brishz.dash bello
## @duplicateCode/jhsd99wiw3i3hehiajh:
if test -z "$NIGHTDIR" ; then
    NIGHTDIR=~/scripts # @hardcoded bash has no way of determining this by itself
    if ! test -d "$NIGHTDIR" ; then
        unset NIGHTDIR
        echo "NIGHTDIR not found"
    fi
fi
##
source "$NIGHTDIR"/zshlang/basic/conditions.zsh
##
export TERM=xterm-256color # @surprise
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
