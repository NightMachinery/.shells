##
function phone2iran {
    cat-paste-if-tty | sd '\s+' '' | sd '^(?:\+|00?)?98' '0' | cat-copy-if-tty
}
##
