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
inargse() {
	mdoc Feeds the function from stdin if no arguments supplied MAGIC
	# An enhancer
	eval "$1" "$(gq "$(in-or-args "${@:2}")")"
}
inargsfe() { eval "$1" "$(gq "${(f@)"$(in-or-args "${@:2}")"}")" }
inargs0e() { eval "$1" "$(gq "${(0@)"$(in-or-args "${@:2}")"}")" }
inargs() {
	reval "$@" "$(in-or-args)"
}
inargsf() { reval "$@" "${(f@)"$(in-or-args)"}" }
inargs0() { reval "$@" "${(0@)"$(in-or-args)"}" }
inargss() { reval "$@" "${="$(in-or-args)"}" }
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
    trap false INT TERM
    functions[$fn]=$body
    reval "${cmd[@]}"
    local e=$?
    # ec Trying to restore after command execution ...
    # geval "$restore"
    test -n "$origbody" && functions[$fn]="$origbody" || unfunction $fn
    trap - INT TERM
    return $e
}
fnswap() {
	fnrep "$1" "$2 "'"$@"' "$@[3,-1]"
}
fnrepvc() {
    fnrep git "vcsh $1"' "$@"' "${@[2,-1]}"
}
