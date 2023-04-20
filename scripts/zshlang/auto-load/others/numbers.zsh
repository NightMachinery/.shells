##
function num-base-convert-py {
    local n from_base="${num_base_convert_from:-16}" to_base="${num_base_convert_to:-10}"
    n="$(in-or-args "$@")"
    n=(${(@f)n})

    local i
    for i in $n[@] ; do
        num_base_convert.py "$n" "$from_base" "$to_base"
    done
}
@opts-setprefix num-base-convert-py num_base_convert

aliasfn dice2decimal dice2decimal.py
##
function decimal2hex-py {
    # @todo1 @broken
    ##
    @opts from 10 to 16 @ num-base-convert-py "$@"
}
aliasfn decimal2hex decimal2hex-py

function hex2decimal-py {
    @opts from 16 to 10 @ num-base-convert-py "$@"
}
aliasfn hex2decimal hex2decimal-py
##
function numfmt-humanfriendly-bytes {
    numfmt --to=iec-i --suffix=B "$@"
}
aliasfn numfmt-bytes numfmt-humanfriendly-bytes

function numfmt-humanfriendly {
    numfmt --to=si --round=nearest "$@"
}

function numtfmt-comma {
    in-or-args "$@" |
        perl -lpe 's/(?<=\d)(?=(\d{3})+\b)/,/g' |
        cat-copy-if-tty
}
##
