##
function history-all {
    #: @seeAlso [agfi:hist-last]
    ##
    builtin fc -rl 1 |
        perl -lpe 's/^\s*\S+\s*//g'
}
##
function invocation-save {
    local dest="$1" cmd=("${@[2,-1]}")
    assert-args dest cmd

    if [[ "$dest" != *.txt ]] ; then
        dest="${dest}_invocations.txt"
    fi

    { gquote "$cmd[@]" ; ec } >> "$dest"
    duplicates-clean-file-inplace "$dest"
}
##
