##
function whitespace-p {
    in-or-args2 "$@"
    if ecn "$inargs[*]" | silent command rg '\S' ; then
        return 1
    else
        return 0
    fi
}
function whitespace-is {
    whitespace-p "$@"
}
##
