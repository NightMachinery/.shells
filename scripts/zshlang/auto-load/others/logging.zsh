##
function log-to {
    local log_file="${1}" ; shift
    local cmd=("$@")
    assert-args log_file @RET

    {
        ec-sep-h
        ecdate "logging: $(gquote "${cmd[@]}")"
        ec
        reval "$@"
        ec
    } |& sync-append-in "$log_file"
}
##
