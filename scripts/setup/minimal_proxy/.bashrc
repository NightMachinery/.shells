###
ec() {
    printf "%s\n" "$*"
}

gquote () {
    ec "${*@Q}"
}

source ~/.shared.sh
##
HISTCONTROL=ignoreboth
#: don't put duplicate lines or lines starting with space in the history.
#: See bash(1) for more options

shopt -s histappend
#: append to the history file, don't overwrite it

PROMPT_COMMAND="builtin history -a ; builtin history -r ; ${PROMPT_COMMAND}"
#: save and reload the history on each prompt

HISTSIZE=1000
HISTFILESIZE=2000
#: for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
###
