#!/usr/local/bin/bash

echo starting with "$@"
args=("$@")

if test -z "$BASHRC_LOADED" ; then
    echo Loading bashrc manually
    source ~/.bashrc
fi

brishzq.zsh insubshell zopen "${args[@]}" & # idk what would happen on print, we can use awaysh if necessary

# brishzq.zsh ec test
# brishz.dash ec test2

# echo exiting
