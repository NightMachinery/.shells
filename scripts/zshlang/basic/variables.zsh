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
function array-contains {
    #:   Checks if the specified array contains all the given literals.
    #:   If no literals are provided, it returns success.
    #: Usage:
    #:   array-contains array_name [literal1 literal2 ...]
    #: Returns:
    #:   0 (success) if all literals are present in the array or no literals are provided.
    #:   1 (failure) otherwise.
    #:
    #: [[id:a98b2b0f-2ca2-4ed8-a4bd-8a8c4c017c75][@tests array-contains]]
    ##
    local array_name literals array literal

    if (( $# < 1 )); then
        ecerr "Usage: array-contains array_name [literal1 literal2 ...]"
        return 1
    fi

    array_name="$1"
    shift
    literals=("$@")

    #: If no literals are provided, return success
    if (( $# == 0 )); then
        return 0
    fi

    ensure-array "$array_name"

    #: Retrieve the contents of the array using indirect parameter expansion
    array=("${(P@)array_name}")

    #: Iterate over each literal to check its presence in the array
    for literal in "${literals[@]}"; do
        if (( ${array[(Ie)$literal]} == 0 )); then
            #: Literal not found in the array
            return 1
        fi
    done

    #: All literals were found in the array
    return 0
}
alias in-array=array-contains
##
