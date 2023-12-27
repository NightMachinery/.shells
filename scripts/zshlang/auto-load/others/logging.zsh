##
function log-to {
    local log_file="${1}" ; shift
    local cmd=("$@")
    assert-args log_file @RET

    {
        ec-sep-h
        edPre="* " ecdate "logging: $(gquote "${cmd[@]}")"
        ec
        reval "$@"
        ec
        ec-sep-h
    } |& sync-append-in "$log_file"
}
##
