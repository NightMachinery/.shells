##
function phone-number-ir-to-98 {
    in-or-args "$@" |
        trimsed |
        perl -lpe 's/^(*nla:\+)0?/+98/g' |
        cat-copy-if-tty
}
aliasfn phone98 phone-number-ir-to-98

function phone2iran {
    cat-paste-if-tty | sd '\s+' '' | sd '^(?:\+|00?)?98' '0' | cat-copy-if-tty
}
##
