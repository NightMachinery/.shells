##
function qtconsole-launch {
    #  [[id:5776a712-5440-41ef-8800-8508c9e05454][qtconsole/cheatsheets/install]]
    ##
    local kernel_name="${1:-$(jupyter-kernel-julia-name-get)}" opts=("${@[2,-1]}") # @duplicateCode/e8fda9d37f0228e6a43f2d814269c5b5
    local env="${qtconsole_launch_env:-p39}" # @hardcoded
    local stylesheet="${qtconsole_launch_stylesheet}"

    if test -n "$env" ; then
        reval-ec conda-activate "$env"
    fi

    reval-ec jupyter qtconsole \
        --style solarized-light \
        --editor="$commands[emacs_nowait.dash] {filename}" \
        --stylesheet="$stylesheet" \
        --kernel="$kernel_name" \
        --gui-completion='ncurses' \
        "${opts[@]}" --debug
}

function qtconsole-launch-daemon {
    local kernel_name="${1:-$(jupyter-kernel-julia-name-get)}" opts=("${@[2,-1]}") # @duplicateCode/e8fda9d37f0228e6a43f2d814269c5b5

    local name
    name="qtconsole-${kernel_name}-$(dateshort)"
    name="$(ec "$name" | str2tmuxname)"

    reval-ec tmuxnewsh2 "$name" \
        qtconsole_launch_env="$qtconsole_launch_env" \
        qtconsole_launch_stylesheet="$(cp-tmp "$qtconsole_launch_stylesheet")" \
        qtconsole-launch "$@"
}
@opts-setprefix qtconsole-launch-daemon qtconsole-launch
aliasfn qtc qtconsole-launch-daemon
@opts-setprefix qtc qtconsole-launch
##
