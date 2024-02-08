##
function printz-quoted {
    printz "$(gq "$@")"
}

function printz {
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
function plot-stdin-datadash {
    ##
    #: https://github.com/seenaburns/stag
    ##
    # ttyplot -s 20 -u '%' # mediocre, buggy
    ##
    datadash --average-line
    #: @install https://github.com/keithknott26/datadash/issues/5
    ##
}
aliasfn plot-stdin plot-stdin-datadash

function plot-stdin-gnuplot {
    feedgnuplot --stream --terminal 'dumb 120,30' --lines
    #: outputs new plots instead of updating the creen.
    #: `gnuplot -e 'set terminal'` for a list of outputs
}

function plot-stdin-asciigraph {
    asciigraph -r # usable but no stats and wrong height detection. No fixed scale. `goi github.com/guptarohit/asciigraph/cmd/asciigraph`
}

function plot-stdin-histogram-py {
    #: * @usage
    #: ** See [agfi:grade-stats]
    ##
    histogram.py "$@"
}

function plot-stdin-histogram-gnuplot {
    #: @broken
    #: [[https://gnuplotting.org/tag/histogram/][histogram « Gnuplotting]]
    ##
    ecgray 'Use `plot-stdin-histogram-py` instead.'

    gnuplot -e "set term dumb ${COLUMNS}, 17; set autoscale; set boxwidth 0.5; plot '-' with boxes notitle"
}

function plot-stdin-histogram-octave {
    #: @broken
    ##
    ecgray 'Use `plot-stdin-histogram-py` instead.'

    octave --no-window-system --eval "data = stdin(); hist(data); xlabel('Bins'); ylabel('Frequency'); title('Histogram of Data from stdin'); print -dpng 'histogram.png'"
}
##
