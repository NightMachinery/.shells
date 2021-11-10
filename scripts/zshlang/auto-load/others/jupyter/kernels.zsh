##
function h-jupyter-kernel-julia-name-get {
    jupyter kernelspec list | rget '^\s*(julia-\d+\.\d+)\s+' | head -n 1
}
aliasfn jupyter-kernel-julia-name-get eval-memoi h-jupyter-kernel-julia-name-get
##
function jupyter-kernel-launch {
    local kernel="${1}" name="${2}"
    assert-args kernel name @RET

    if ! [[ "$name" == *.json ]] ; then
        name+='.json'
    fi

    local dir=~/tmp/jupyter_kernels
    ensure-dir "${dir}/"
    reval-ec tmuxnewsh2 "jupyter-kernel-${name:r}" \
        reval-ec jupyter kernel --kernel="$kernel" \
        --KernelManager.connection_file="${dir}/${name}" \
        --KernelManager.autorestart=True \
        --KernelApp.log_level=DEBUG \
        --Application.log_level=DEBUG \
        --JupyterApp.log_level=DEBUG
}

function jupyter-kernel-launch-julia {
    local name="${1:-j1}"

    jupyter-kernel-launch "$(jupyter-kernel-julia-name-get)" "$name"
}
##
