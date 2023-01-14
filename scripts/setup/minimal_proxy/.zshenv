###
export TERM="xterm-256color"

export HISTFILE="${HOME}/.zsh_history"
export HISTSIZE=100000
###
setopt interactivecomments
###
export VISUAL="vim"
export EDITOR="${VISUAL}"
##
export GOPATH="$HOME/go"
export GOBIN="$GOPATH/bin"
export PATH="$PATH:/usr/local/go/bin"
export PATH="${PATH}:${GOBIN}"
export PATH="${PATH}:${HOME}/bin"
export PATH="${PATH}:${HOME}/.local/bin"
export PATH="${PATH}:${HOME}/.local/opt/brew/bin"
###
function retry {
    local retry_sleep="${retry_sleep:-0.1}"

    until eval "${(q+@)@}"
    do
        sleep "$retry_sleep"
    done
}
##
tmuxnew () {
    tmux kill-session -t "$1" &> /dev/null
    tmux new -d -s "$@"
}
###
http-static-py () {
    python -m http.server "${1:-8000}"
}
##
