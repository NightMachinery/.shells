function enh-savename() {
    # TODO Adding a way to keep track of all saved names would be good.
    mdoc "$0 <name of original function> <its renamed version after enhancement>" MAGIC

    typeset -A -g enhSavedNames
    test -n "${enhSavedNames[$1]}" || enhSavedNames[$1]="$2"
}
function _aliasfn() {
    : "ruu might be needed. Example: aliasfn hi ruu someVar=12"
    local name="$1"
    local body="$@[2,-1]"
    functions[$name]="$body "'"$@"'
    enh-savename "$name" "$2"
}
# enh-savename aliasfn _aliasfn # redundant, we will auto-erase noglob ourselves
alias aliasfn='\noglob _aliasfn'
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
function createglob() {
    local from="$1"
    local to="$2"
    { test -z "$from" || test -z "$to" } && {
        ecerr "$0: insuffient arguments supplied. (needs 2)"
        return 1
    }
    eval $to'="*.(${(j.|.)'$from'})(.D)"'
}
