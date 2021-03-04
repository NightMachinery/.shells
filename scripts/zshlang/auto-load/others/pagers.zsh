export LESSMIN='-RiF --mouse --wheel-lines=3'
alias lmin='LESS=$LESSMIN '
function less-min() {
    LESS=$LESSMIN less "$@"
}
function jqless() {
    jq --color-output | less
}
