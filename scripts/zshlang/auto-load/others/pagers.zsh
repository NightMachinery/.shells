##
export GIT_PAGER="delta --light --theme 'Solarized (light)'"
##
export LESSMIN='-RiF --mouse --wheel-lines=3 -j.3'
# F: --quit-if-one-screen ; R: maintain the ANSI colour sequences; i: smartcase searches (all lower=ignore case);  -N or --LINE-NUMBERS Causes a line number to be displayed at the beginning of each line in the display.
#
# -j.5 causes search matches to be centered at the specified fraction of the screen

alias lmin='LESS=$LESSMIN '
function less-min() {
    LESS=$LESSMIN less "$@"
}

## @personal :
export LESS="${LESSMIN} -N"
isSSH && LESS="-RiNF"
export PAGER="$commands[less]"
##
function jqless() {
    jq --color-output | less
}
alias jql=jqless
##
