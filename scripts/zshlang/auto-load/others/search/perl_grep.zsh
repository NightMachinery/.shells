##
function grep-before-only {
    : "Usage: lines_before_count pattern ..."

    if (( $#@ < 2 )) ; then
        ecerr "$0: not enough arguments supplied."
        return 1
    fi

    grep_before_only.pl "$@" | tac
}
##
