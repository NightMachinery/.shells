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
    local inargs
    in-or-args3 "$@" @RET

    local i
    for i in ${inargs[@]} ; do
        numfmt --to=iec-i --suffix=B "$i"
    done
}
aliasfn numfmt-bytes numfmt-humanfriendly-bytes

function numfmt-humanfriendly {
    local inargs
    in-or-args3 "$@" @RET

    numfmt --to=si --round=nearest  ${inargs[@]}
}

function numfmt-comma {
    #: * @tests
    #: ** `ec '$999.21 + $1000.0000 + $0.0001 + $0.003' | numfmt-comma`
    #:      $999.21 + $1,000.000,0 + $0.000,1 + $0.003
    ##
    in-or-args "$@" |
        perl -lpe 's/(*plb:\.(?:\d{3}))(*pla:\d)/,/g' | #: This formats the digits after the decimal point. You can comment it to disable this.
        perl -lpe 's/(?:(*nlb:\.\d{1,244})(*plb:\d)(*pla:(\d{3})+\b))/,/g' |
        cat-copy-if-tty
}
##
function rial2human {
    local inargs
    in-or-args3 "$@" @RET
    re rial2human.py ${inargs[@]}
}

function toman2human {
    local inargs
    in-or-args3 "$@" @RET
    re "rial2human.py --from-toman" ${inargs[@]}
}

function h-human2number-preview {
    #: Usage: h-human2number-preview <preview_fn> [human2number.py flags ...] [inputs ...]
    #: Inputs come from args, stdin, or pbpaste (via in-or-args3).
    ##
    local preview="$1" ; shift
    local pyflags=()
    while (( $# )) && [[ "$1" == --* ]] ; do pyflags+=("$1") ; shift ; done

    local inargs
    in-or-args3 "$@" @RET

    local i
    for i in ${inargs[@]} ; do
        human2number.py "${pyflags[@]}" "$i"
    done > >( { colorfg "$gray[@]" ; "$preview" ; resetcolor } >&2) | cat-copy-if-tty
}

function human2rial {
    h-human2number-preview rial2human --rial "$@"
}

function human2toman {
    h-human2number-preview toman2human "$@"
}

aliasfn toman2rial human2rial

function rial2toman {
    #: pure numeric rial->toman; rial never goes through the human parser
    ##
    local inargs
    in-or-args3 "$@" @RET

    local i
    for i in ${inargs[@]} ; do
        i="${i//,/}"
        if [[ "$i" != *.* ]] && (( i % 10 == 0 )) ; then
            ec $(( i / 10 ))
        else
            ec $(( i / 10. ))
        fi
    done | cat-copy-if-tty
}
##
