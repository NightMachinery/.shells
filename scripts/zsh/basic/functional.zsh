function labeled() {
    #  zip enumerate
    ec "${@[2,-1]}: $(reval "$@")"
}
##
function fnrep() {
    : 'Replaces a function temporarily during <cmd>: <fn> <new body> <cmd>'
    : 'WARNING: Currently does not accommodate our own macro-enhancers like reify.'

    local fn="$1"
    local body="$2"
    local cmd=( "${@[3,-1]}" )
    # local origbody="_origbody_$fn_$(uuidpy)"
    # eval $origbody'=$functions['$fn'] || '$origbody'=""'
    # local restore="_restore_$1_$(uuidpy)"
    # functions[$restore]='{ test -n "$'$origbody'" && functions['$fn']=$'$origbody' || unfunction '$fn' } ; trap - INT TERM ; unfunction '$restore' ; return 1'
    # functions[$restore]='return 1'
    local origbody origalias
    origbody=$functions[$fn] || $origbody=''
    origalias=$aliases[$fn] || $origalias=''

    local e=1
    {
        # trap false INT TERM
        unalias $fn &>/dev/null
        functions[$fn]=$body
        reval "${cmd[@]}"
        e=$?
    } always {
        # ec Trying to restore after command execution ...
        # geval "$restore"
        test -n "$origbody" && functions[$fn]="$origbody" || unfunction $fn
        test -n "$origalias" && aliases[$fn]="$origalias"
        # trap - INT TERM
    }
    return $e
}
function fnswap() {
    fnrep "$1" "$2 "'"$@"' "$@[3,-1]"
}
function fnrepvc() {
    fnrep git "vcsh $1"' "$@"' "${@[2,-1]}"
}
##
