##
function invocation-save {
    local dest="$1" cmd=("${@[2,-1]}")
    assert-args dest cmd

    dest="${dest}_invocations.txt"

    { gquote "$cmd[@]" ; ec } >> "$dest"
    duplicates-clean-file-inplace "$dest"
}
##
