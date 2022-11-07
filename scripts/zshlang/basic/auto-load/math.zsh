##
function num-min {
    #: =bound= from =num-utils=
    ##
    local res
    res="$(serr command bound -l)" @RET
    #: prints an error when there are no numbers in its input

    test -n "$res" && ec "$res" || return 1
}

function in-sum {
    : "@alt num-utils: average, bound, interval, normalize, numgrep, numprocess, numsum, random, range, round"

    gawk '{s+=$1} END {printf "%.0f\n", s}'
    # https://stackoverflow.com/questions/450799/shell-command-to-sum-integers-one-per-line
}
##
