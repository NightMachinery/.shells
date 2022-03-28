##
function les() {
    if (( ${#@} == 0 )) ; then
        eval-ec "$(hist-last 1)" |& less
    else
        reval "$@" |& less
    fi
}

function lesh() { les "$@" --help }
##
