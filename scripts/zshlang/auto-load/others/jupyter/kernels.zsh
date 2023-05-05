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
function jupyter-alive-p {
    #: [[id:ae5c23b4-9401-4d53-8a70-b89b46aa9ea9][API/password protected]]
    #: currently auth doesn't work
    ##
    local server="$1"
    assert-args server @RET

    curl --head "${server}"
}

function jupyter-alive-p-v1 {
    #: [[id:ae5c23b4-9401-4d53-8a70-b89b46aa9ea9][API/password protected]]
    #: @broken
    ##
    local server="$1" password="$2"
    # token="$2"
    assert-args server @RET

    if ! [[ "$server" =~ '.*/$' ]]; then
        server+='/'
    fi

    local tmp
    tmp="$(gmktemp)"

    if test -n "$password" ; then
        login_response="$(curl -s -X POST -c "$tmp" "${server}login" -d "password=${password}")"
        typ login_response
    fi

    # --header "Authorization: token ${token}"
    reval-ec curl -c "$tmp" --silent --request GET "${server}api/status"
    # curl --head "$1"
}
##
