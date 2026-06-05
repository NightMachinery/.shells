## Minimal tmux helpers shared by public plugins and local tmux helpers.
#: @duplicateCode/0c8b9d0226cdfb4f5bc0a9ea735089df
function tmuxnew {
    #: @todo0 integrate =str2tmuxname=
    ##
    if (( ${+aliases[silent]} )) ; then
        silent tmux kill-session -t "$1" || true
    else
        command tmux kill-session -t "$1" &> /dev/null || true
    fi
    command tmux new -d -s "$@"
}
