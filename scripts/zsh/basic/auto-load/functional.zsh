function combine-funcs() {
    # Combine multiple functions into one named by $1; The result will run all functions with $@.
    local tmp321_string="function $1() { "
    for i in "${@:2}"
    do
        tmp321_string="$tmp321_string""$i "'"$@"; '
    done
    tmp321_string="$tmp321_string""}"
    # echo "$tmp321_string"
    eval "$tmp321_string"
}
mapg() {
    local args=("$@")
    local i res=''
    for i in "$@[2,-1]"
    do
        set -- "$i"
        res+="$(eval ec "$args[1]")"${mg_sep:-$'\n'}
    done
    ec "$res"
}
mapln() { mg_sep=$'\n' mapg "$@" }
mapnul() { mg_sep=$'\0' mapg "$@" }
revargs() {
	mdoc reverse args: "$0" '<function>' arguments... MAGIC
	# (O = reverse of the order specified in the next flag, a = normal array order).
	eval "$1" "$(gq "${(Oa)@[2,-1]}")"
}
##
# inargse() {
# 	mdoc Feeds the function from stdin if no arguments supplied MAGIC
# 	# An enhancer
# 	eval "$1" "$(gq "$(in-or-args "${@:2}")")"
# }
# inargsfe() { eval "$1" "$(gq "${(f@)"$(in-or-args-arrN "${@:2}")"}")" }
# inargs0e() { eval "$1" "$(gq "${(0@)"$(in-or-args-arr0 "${@:2}")"}")" }
alias inargsEf="fsep=$'\n' inargsE-gen"
alias inargsE0="fsep=$'\0' inargsE-gen"
alias inargsEc="fsep='' inargsE-gen" # splits by char
alias inargsEs="fsep='IFS' inargsE-gen"
alias inargsEa="fsep='ALL' inargsE-gen"
inargsE-gen() {
    local cmd="$1"
    local args=( "${@[2,-1]}" )
    if test -z "$args[*]" ; then
        inargs-gen "$cmd"
    else
        eval "$cmd $(gq "$args[@]")"
    fi
}
alias inargsf="fsep=$'\n' inargs-gen"
alias inargs0="fsep=$'\0' inargs-gen"
alias inargsc="fsep='' inargs-gen" # splits by char
alias inargss="fsep='IFS' inargs-gen"
alias inargsa="fsep='ALL' inargs-gen"
inargs-gen() {
    local sep="${fsep}"
    local noSkipEmpty="$inargsRunEmpty"
    local cmd="$1"
    local args args0=( "${@[2,-1]}" )

    doc "We intentionally drop empty args"
    args="${$(</dev/stdin ; print -n .)[1,-2]}"
    if [[ "$sep" == IFS ]] ; then
        args=( ${=args} )
    elif [[ "$sep" == ALL ]] ; then

    else
        args=( ${(@ps:$sep:)args} )
    fi
    args=( "${args0[@]}" ${args[@]} )

    # (( $#args == 0 ))
    { test -z "$args[*]" && test -z "$noSkipEmpty" } || eval "$cmd $(gq "$args[@]")"

}
##
# test: arr0 {1..10} | filter0 '(){ (( $1 <= 3 )) }' |inargs0 arger
alias filter="fisep=$'\n' fsep=$'\n' filterE-gen"
alias filter0="fisep=$'\0' fsep=$'\0' filterE-gen"
function filterE-gen() {
    inargsE-gen "filter-gen $(gq "$1")" "${@[2,-1]}"
}
function filter-gen() {
    local cmd="$1"
    local items=( "$@[2,-1]" )
    local sep="${fisep}"
    
    local i out=()
    for i in "$items[@]"
    do
        if seval "${cmd} $(gq "$i")" ; then
            out+="$i"
        fi
    done
    print -nr -- "${(pj:$sep:)out}"
}
##
fnrep() {
    doc 'Replaces a function temporarily during <cmd>: <fn> <new body> <cmd>'
    local fn="$1"
    local body="$2"
    local cmd=( "${@[3,-1]}" )
    # local origbody="_origbody_$fn_$(uuidpy)"
    # eval $origbody'=$functions['$fn'] || '$origbody'=""'
    # local restore="_restore_$1_$(uuidpy)"
    # functions[$restore]='{ test -n "$'$origbody'" && functions['$fn']=$'$origbody' || unfunction '$fn' } ; trap - INT TERM ; unfunction '$restore' ; return 1'
    # functions[$restore]='return 1'
    local origbody
    origbody=$functions[$fn] || $origbody=''

    local e=1
    {
        # trap false INT TERM
        functions[$fn]=$body
        reval "${cmd[@]}"
        e=$?
    } always {
        # ec Trying to restore after command execution ...
        # geval "$restore"
        test -n "$origbody" && functions[$fn]="$origbody" || unfunction $fn
        # trap - INT TERM
    }
    return $e
}
fnswap() {
	fnrep "$1" "$2 "'"$@"' "$@[3,-1]"
}
fnrepvc() {
    fnrep git "vcsh $1"' "$@"' "${@[2,-1]}"
}
labeled() {
    #  zip enumerate
    ec "${@[2,-1]}: $(reval "$@")"
}
