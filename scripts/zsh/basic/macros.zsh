function aliasfn() {
    : "ruu might be needed. Example: aliasfn hi ruu someVar=12"
    local name="$1"
    local body="$@[2,-1]"
    functions[$name]="$body "'"$@"'
}
function aliasfn-classic() {
    local args=( "$@" )
    [[ "$args[*]" =~ '\s*([^=]+)=(.*[^\s])\s*' ]] || { echo invalid alias: "$args[*]" >&2 ; return 1 }
    run-on-each dvar args match
    aliasfn "$match[1]" "$match[2]"
}
function aliassafe() {
    builtin alias "$@"
    aliasfn-classic "$@"
}
