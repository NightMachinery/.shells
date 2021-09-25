##
function var-set-p {
    if (( ${(P)+1} == 1 )) ; then
        return 0 # set
    else
        return 1 # not set
    fi
}

function ensure-array {
    local i
    for i in $@ ; do
        # if ! var-set-p "$i" ; then
        if (( ${#${(P)i}} == 0 )) ; then
            typeset -ag "$i"
        fi
    done
}

function ensure-array-var() {
    ensure-array "$@"
}
##

##
