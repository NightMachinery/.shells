##
function do-rnd() {
    reval "$(rndarr "$@")"
}
##
function fndef-unquoted() {
    functions[${1:?}]="${@:2}"
}
fndef-uq() { fndef-unquoted "$@" }
function fndef() {
    fndef-unquoted "$1" "$(gq "${@:2}")"
}
## doesn't work because the inner vars do not expand (naturally). But you can still use this trick instead of using fndef.
# function fndef2() {
#     function "$1" () {
#         "${@:2}"
#     }
# }
##
function lambda-unquoted() {
    unset lambda_out
    local name="$(uuidm)_$EPOCHREALTIME"
    fndef-unquoted "$name" "$@" || {
        ec "false"
        return 1
    }
    # ec "$name"
    lambda_out="$name"
}
lambda-uq() { lambda-unquoted "$@" }
function lambda() {
    unset lambda_out
    lambda-unquoted "$(gq "$@")"
}
##
function labeled() {
    #  zip enumerate
    ec "$(gq "${@}"): $(reval "$@")"
    # ec "${@[2,-1]}: $(reval "$@")"
}
##
function fnrep() {
    # : 'Replaces a function temporarily during <cmd>: <fn> <new body> <cmd>'
    # : 'WARNING: Currently does not accommodate our own macro-enhancers like reify.'

    local fn="$1"
    local body="$2"
    local cmd=( "${@[3,-1]}" )

    local origfn
    origfn="fn_$(uuidm)"
    functions[$origfn]=$functions[$fn]
    # ec "origfn: $functions[$origfn]"

    local origbody origalias
    origbody=$functions[$fn] || $origbody=''
    origalias=$aliases[$fn] || $origalias=''

    local e=1
    {
        # trap false INT TERM
        unalias $fn &>/dev/null

        alias fnsuper="$origfn"
        functions[$fn]=$body
        # ec "fn: $functions[$fn]"
        unalias fnsuper

        reval "${cmd[@]}"
        e=$?
    } always {
        # ec Trying to restore after command execution ...
        # geval "$restore"
        test -n "$origbody" && functions[$fn]="$origbody" || unfunction $fn
        test -n "$origalias" && aliases[$fn]="$origalias"
        unfunction $origfn
        # trap - INT TERM
    }
    return $e

    ## tests
    # `fnrep arrN 'ecn "before|" ; fnsuper "$@" ; ecn "|after" ' arrN Hi, $'\t'Alice'!' `
    # `fnrep arrN 'ecn "<" ; fnsuper "$@" ; ecn ">" ' fnrep arrN 'ecn "before|" ; fnsuper "$@" ; ecn "|after" ' arrN Hi, $'\t'Alice'!'`
    ##
}
function fnswap() {
    fnrep "$1" "$2 "'"$@"' "$@[3,-1]"
}
function fnrepvc() {
    fnrep git "vcsh $1"' "$@"' "${@[2,-1]}"
}
##
