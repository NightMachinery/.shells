##
export NIGHTDIR=~/scripts
##
shopt -s globstar
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
source "$NIGHTDIR"/zsh/basic/conditions.zsh
##
source ~/.shared.sh
### Interactive
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
##
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion




