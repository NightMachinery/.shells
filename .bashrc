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
#: Do not overwrite the TERM the client sent. Hardcoding it meant every shell
#: here reported xterm-256color whatever terminal was attached, so a kitty
#: client lost its own terminfo even on hosts that had the entry installed.
#: Downgrade only a TERM this host cannot actually resolve.
#: @see ~/scripts/docs/tmux-termux-truecolor.md
#: @duplicateCode of the same guard in setup/minimal_proxy/.shared.sh, which
#: must stay standalone and cannot call into zshlang.
if [ -z "${TERM}" ] ; then
    export TERM=xterm-256color
elif command -v infocmp > /dev/null 2>&1 && ! infocmp "${TERM}" > /dev/null 2>&1 ; then
    export TERM=xterm-256color
fi
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
