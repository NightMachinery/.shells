##
function num-stats {
    {
        > >(labeled average) \
        > >(labeled num-std) \
        > >(labeled num-max) \
        > >(labeled num-min)
     } |
        cat
    #: The ending =cat= makes it wait to finish. I don't know why it doesn't wait otherwise.
}

function grade-stats {
    local data
    data="$(in-or-args "$@")" @RET

    local icat_v=n

    local data_ge_10
    data_ge_10="$(ec "$data" | perl -nle '(print $_ <= 20 ? $_ : 20) if $_ >= 10')" @TRET

    local data_ge_19_5
    data_ge_19_5="$(ec "$data" | perl -nle 'print $_ if $_ >= 19.49 and /\d+/')" @TRET

    local data_le_10
    data_le_10="$(ec "$data" | perl -nle 'print $_ if $_ <= 10 and /\d+/')" @TRET


    ec "$data" | plot-stdin-histogram-py --dpi=320 -b=1 - | icat

    ec $'\n'"<10:"
    ec "$data_le_10" | prefixer -a '  '
    ec $'\n'">=19.5:"
    ec "$data_ge_19_5" | prefixer -a '  '

    ec
    ec "$data_ge_10" | {
        ec ">=10 min: $(num-min)"
    }
    ec "$data" | labeled num-max

    ecgray $'\n'"$0: Removing less than 10 and clamping to 20 for average and STD ..."
    ec "$data_ge_10" | labeled average
    ec "$data_ge_10" | labeled num-std
}

function grade-stats-v1 {
    ecgray "$0: Removing less than 10 and clamping to 20 for average and STD ..."

    in-or-args "$@" |
        {
            > >(
                plot-stdin-histogram-py --dpi=320 -b=1 - | icat
                #: A bigger DPI makes the image larger.
            ) \
                > >(
                perl -nle '(print $_ <= 20 ? $_ : 20) if $_ >= 10' |
                    > >(labeled average) \
                        > >(labeled num-std) \
                        > >(ec ">=10 min: $(num-min)")
            ) \
                > >(labeled num-max) \
                > >(
                ec ">=19.5:"
                perl -nle 'print $_ if $_ >= 19.49 and /\d+/' |
                    prefixer -a '  '
                ec
            ) \
                > >(labeled num-max) \
                > >(
                ec "<10:"
                perl -nle 'print $_ if $_ <= 10 and /\d+/' |
                    prefixer -a '  '
                ec
            )
            # > >(labeled num-min)
        } |
        cat
}

function num-min {
    #: =bound= from =num-utils=
    ##
    local res
    res="$(serr command bound -l)" @RET
    #: prints an error when there are no numbers in its input

    test -n "$res" && ec "$res" || return 1
}

function num-max {
    #: =bound= from =num-utils=
    ##
    local res
    res="$(serr command bound)" @RET
    #: prints an error when there are no numbers in its input

    test -n "$res" && ec "$res" || return 1
}

function num-std {
    octave --no-window-system --eval 'disp(std(scanf("%f"), 1))'
    #: [[https://octave.sourceforge.io/octave/function/std.html][Function Reference: std]]
    #: The argument =opt= determines the type of normalization to use. Valid values are
    #: - 0: :: normalize with /N-1/, provides the square root of the best unbiased estimator of the variance [default]
    #: - 1: :: normalize with /N/, this provides the square root of the second moment around the mean
}

function in-sum {
    : "@alt num-utils: average, bound, interval, normalize, numgrep, numprocess, numsum, random, range, round"

    gawk '{s+=$1} END {printf "%.0f\n", s}'
    # https://stackoverflow.com/questions/450799/shell-command-to-sum-integers-one-per-line
}
##
