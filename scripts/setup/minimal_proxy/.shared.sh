##
export TERM="xterm-256color"

export TZ='Asia/Tehran'

export VISUAL="vim"
export EDITOR="${VISUAL}"
export ALTERNATE_EDITOR="" #: Causes Emacs to start a daemon if one is not found.

export DOOMDIR=~/doom.d

export LESSMIN='-RiF --mouse --wheel-lines=3 -j.3'
##
psource () {
    if [[ -r "$1" ]]
    then
        source "$@"
    fi
}
##
psource ~/.config/envman/PATH.env

export GOPATH="$HOME/go"
export GOBIN="$GOPATH/bin"
export PATH="/usr/local/go/bin:${PATH}"
export PATH="${GOBIN}:${PATH}"

export PATH="${HOME}/bin:${PATH}"

export PATH="${HOME}/.emacs.d/bin:${PATH}"

export PATH="${HOME}/.local/bin:${PATH}"
export PATH="${HOME}/.local/opt/brew/bin:${PATH}"
export PATH="${HOME}/.local/opt/brew/sbin:${PATH}"
##
alias wh='which'

alias bi='brew install'
alias pi='pip install --upgrade'

alias ll='ls -alh'
alias l='ls -ah'

alias gcl='git clone --recursive'
alias ga='git add'
alias gc='git commit'
alias gco='git checkout'
alias gl='git pull'
alias gp='git push'
alias grv='git remote -v'
alias gss='git status'

glola () {
    LESS=$LESSMIN git log --graph --pretty=format:'%Cred%h%Creset %C(yellow)%ad%Creset %Cgreen(%cr)%Creset %s %C(yellow)%d%Creset %C(bold blue)<%an>%Creset' --date=short --abbrev-commit --all
}
##
reval() {
    eval "$(gquote "$@")"
}
##
proxy-env-unset () {
    unset ALL_PROXY all_proxy http_proxy https_proxy HTTP_PROXY HTTPS_PROXY
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
export EMACS_SOCKET_NAME="${EMACS_SOCKET_NAME:-${HOME}/tmp/.emacs-servers/server}"

emc-gateway () {
    ALTERNATE_EDITOR="" LOGNAME="$(whoami)" DOOMDIR=~/doom.d emacsclient -t
}
##
function redo2 {
    local count=$1
    shift
    local cmd="${*@Q}"

    echo "repeating cmd: ${cmd}"
    for ((i=1; i<=count; i++))
    do
        eval "$cmd"
    done
}
##
retry () {
    local retry_sleep="${retry_sleep:-0.1}"

    until reval "${@}"
    do
        sleep "$retry_sleep"
    done
}
##
