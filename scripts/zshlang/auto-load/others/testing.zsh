##
function t-failer {
    if [[ "$1" == (5|7) ]] ; then
        ec "$1: false"
        return 1
    else
        ec "$1: true"
        return 0
    fi
}
##
