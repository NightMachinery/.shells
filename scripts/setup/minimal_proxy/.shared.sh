##
export TERM="xterm-256color"

export TZ='Asia/Tehran'

export VISUAL="vim"
export EDITOR="${VISUAL}"
export ALTERNATE_EDITOR="" #: Causes Emacs to start a daemon if one is not found.

export DOOMDIR=~/doom.d

export LESSMIN='-RiF --mouse --wheel-lines=3 -j.3'

export FZF_DEFAULT_OPTS="--bind 'shift-up:toggle+up,shift-down:toggle+down,alt-up:preview-up,alt-down:preview-down,alt-n:next-history,alt-p:previous-history,tab:toggle,shift-tab:toggle+beginning-of-line+kill-line,alt-/:toggle-preview,ctrl-j:toggle+beginning-of-line+kill-line,ctrl-t:top,ctrl-s:select-all,alt-enter:print-query,shift-right:replace-query' --color=light --multi --hscroll-off 99999"
##
psource () {
    if [[ -r "$1" ]]
    then
        source "$@"
    fi
}

bool () {
    local i=$(echo "$1" | tr '[:upper:]' '[:lower:]')
    if [[ "${i}" == "n" ]] || [[ "${i}" == "no" ]] || [[ "${i}" == "0" ]]
    then
        return 1
    else
        test -n "${i}"
        return $?
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
export PATH="/home/linuxbrew/.linuxbrew/bin:${PATH}"
export PATH="/home/linuxbrew/.linuxbrew/sbin:${PATH}"
export HOMEBREW_CURLRC="${HOME}/.curlrc"

export PATH="${HOME}/anaconda/bin:${PATH}"
export PATH="${HOME}/miniconda3/bin:${PATH}"
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

alias pxa87='ALL_PROXY=http://127.0.0.1:1087 all_proxy=http://127.0.0.1:1087 http_proxy=http://127.0.0.1:1087 https_proxy=http://127.0.0.1:1087 HTTP_PROXY=http://127.0.0.1:1087 HTTPS_PROXY=http://127.0.0.1:1087'
alias pxa2096='ALL_PROXY=http://127.0.0.1:2096 all_proxy=http://127.0.0.1:2096 http_proxy=http://127.0.0.1:2096 https_proxy=http://127.0.0.1:2096 HTTP_PROXY=http://127.0.0.1:2096 HTTPS_PROXY=http://127.0.0.1:2096'
# export ALL_PROXY=http://127.0.0.1:2096 all_proxy=http://127.0.0.1:2096 http_proxy=http://127.0.0.1:2096 https_proxy=http://127.0.0.1:2096 HTTP_PROXY=http://127.0.0.1:2096 HTTPS_PROXY=http://127.0.0.1:2096
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
pbcopy-remote() {
    local port="${copy_port:-6030}"

    socat - "tcp:127.0.0.1:${port}"
}

bell-call-remote () {
    local bell_name="${1}"
    local port="${bell_port:-6030}"

    echo "MAGIC_BELL_${bell_name}" |
        copy_port="$port" pbcopy-remote
}
##
python-package-version () {
	local import_p="${import_p:-y}"
	if bool "$import_p"
	then
		import_p=True
	else
		import_p=False
	fi

	python -c "import sys ; import json ; from pynight.common_package import packages_commit_get ; print(json.dumps(packages_commit_get(sys.argv[1:], import_p=${import_p},), indent=2))" "$@"
}
##
clipboard-remote-listen-2file() {
    local port="${1:-6070}"

    tmuxnew "clipboard-listen-2file-${port}" \
        socat -u "TCP-LISTEN:${port},bind=127.0.0.1,fork" 'EXEC:tee '"${HOME}/.remote_clipboard"
    # 'EXEC:zsh -fc \"tee >(pbcopy)\"'
}
##

psource ~/.privateShell
