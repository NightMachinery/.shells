export LESSMIN='-RiF'
alias lmin='LESS=$LESSMIN '
function less-min() {
    LESS=$LESSMIN less "$@"
}
