##
function _aliasfn() {
    : "ruu might be needed. Example: aliasfn hi ruu someVar=12"
    local name="$1"
    local goesto="$2"
    local body="$@[2,-1]"

    functions[$name]="$body "'"$@"'
    enh-savename "$name" "$goesto"
}
# enh-savename aliasfn _aliasfn # redundant, we will auto-erase noglob ourselves
alias aliasfn='\noglob _aliasfn'
function _aliasfnq() {
    local name="$1"
    local goesto="$2"
    local body=("$@[2,-1]")

    fnswap enh-savename true _aliasfn "$name" "$(gq "${body[@]}")"
    enh-savename "$name" "$goesto"
}
alias aliasfnq='\noglob _aliasfnq'

function _aliasfn-ng() {
    aliasfn "$@"
    noglobfn "$1"
}
alias aliasfn-ng='\noglob _aliasfn-ng'
function _aliasfnq-ng() {
    aliasfnq "$@"
    noglobfn "$1"
}
alias aliasfnq-ng='\noglob _aliasfnq-ng'

function aliasfn-classic() {
    local args=( "$@" )
    [[ "$args[*]" =~ '\s*([^=]+)=(.*[^\s])\s*' ]] || { echo invalid alias: "$args[*]" >&2 ; return 1 }
    run-on-each dvar args match
    aliasfn "$match[1]" "$match[2]"
}
aliasfn alifn aliasfn-classic
function aliassafe() {
    builtin alias "$@"
    aliasfn-classic "$@"
}
##
function createglob() {
    local from="$1"
    local to="$2"
    { test -z "$from" || test -z "$to" } && {
        ecerr "$0: insuffient arguments supplied. (needs 2)"
        return 1
    }
    eval $to'="*.(${(j.|.)'$from'})(.D)"'
}
