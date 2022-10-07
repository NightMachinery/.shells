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
###
tmuxnew () {
    tmux kill-session -t "$1" &> /dev/null
    tmux new -d -s "$@"
}
###
