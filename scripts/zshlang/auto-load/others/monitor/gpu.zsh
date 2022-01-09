##
function gpu-usage-darwin() {
    sudo powermetrics --samplers gpu_power -n1 -i200 | rg 'GPU Busy'
}

function gpu-usage-graph() {
    assert isDarwin @RET
    (
        mark-me "$0"
        while true ; do
            gpu-usage-darwin | rget '([\d\.]+)%' || exit $?
        done | plot-stdin || exit $?
    )
}
##
