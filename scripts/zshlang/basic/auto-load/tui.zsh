function printz-quoted() {
    printz "$(gq "$@")"
}
function printz() {
    test -n "$*" && {
        if isI ; then
            print -rz -- "$@ "
        else
            # ec "$@"
            reval-ec "$@"
        fi
    }
}
##
function plot-stdin() {
    # https://github.com/seenaburns/stag
    ##
    # ttyplot -s 20 -u '%' # mediocre, buggy
    ##
    datadash --average-line # install: https://github.com/keithknott26/datadash/issues/5
    ##
    # feedgnuplot --stream --terminal 'dumb 120,30' --lines # outputs new plots instead of updating the creen.
    # `gnuplot -e 'set terminal'` for a list of outputs
    ##
    ## asciigraph -r # usable but no stats and wrong height detection. No fixed scale. `goi github.com/guptarohit/asciigraph/cmd/asciigraph`
}
