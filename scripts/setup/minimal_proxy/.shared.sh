##
export TERM="xterm-256color"
# export TERMINFO=/usr/share/terminfo
# export TERM="xterm+256color"
# export TERM="xterm-color"
# export TERM="xterm-direct"

export TZ='Asia/Tehran'

export LESS='-RiF --mouse --wheel-lines=3 -j.3'

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

cdm () {
    local d="$*"
    mkdir -p -- "$d" && cd -P -- "$d"
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

export PATH="${HOME}/.cargo/bin:${PATH}"
export PATH="${HOME}/.local/bin:${PATH}"
export PATH="${HOME}/.local/opt/brew/bin:${PATH}"
export PATH="${HOME}/.local/opt/brew/sbin:${PATH}"
export PATH="/home/linuxbrew/.linuxbrew/bin:${PATH}"
export PATH="/home/linuxbrew/.linuxbrew/sbin:${PATH}"

export PATH="${HOME}/.config/guix/current/bin:${PATH}"
GUIX_PROFILE="${HOME}/.guix-profile"
psource "$GUIX_PROFILE/etc/profile"

export PATH="${HOME}/anaconda/bin:${PATH}"
export PATH="${HOME}/miniconda3/bin:${PATH}"
##
if test -e "${HOME}/.curlrc" ; then
    export HOMEBREW_CURLRC="${HOME}/.curlrc"
fi
##
alias wh='which'

alias bi='brew install'

alias ll='ls -alh'
alias l='ls -ah'

alias ncdu='ncdu --color off'

alias gcl='git clone --recursive'
alias git-clone-shallow='git clone --recursive --depth=1 --shallow-submodules'
alias gcls='git-clone-shallow'
alias ga='git add'
alias gc='git commit'
alias gco='git checkout'
alias gl='git pull'
alias gp='git push'
alias grv='git remote -v'
alias gss='git status'
alias gd='git diff'

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
alias pxa2097='ALL_PROXY=http://127.0.0.1:2097 all_proxy=http://127.0.0.1:2097 http_proxy=http://127.0.0.1:2097 https_proxy=http://127.0.0.1:2097 HTTP_PROXY=http://127.0.0.1:2097 HTTPS_PROXY=http://127.0.0.1:2097'
# export ALL_PROXY=http://127.0.0.1:2096 all_proxy=http://127.0.0.1:2096 http_proxy=http://127.0.0.1:2096 https_proxy=http://127.0.0.1:2096 HTTP_PROXY=http://127.0.0.1:2096 HTTPS_PROXY=http://127.0.0.1:2096
alias pxateias='ALL_PROXY=http://10.2.32.28:10809 all_proxy=http://10.2.32.28:10809 http_proxy=http://10.2.32.28:10809 https_proxy=http://10.2.32.28:10809 HTTP_PROXY=http://10.2.32.28:10809 HTTPS_PROXY=http://10.2.32.28:10809'
alias pxa='pxa2096'
##
#: @duplicateCode/0c8b9d0226cdfb4f5bc0a9ea735089df
tmuxnew () {
    tmux kill-session -t "$1" &> /dev/null
    tmux new -d -s "$@"
}

tmuxnew-ensure() {
    if ! tmux has-session -t "$1" &> /dev/null ; then
        tmuxnew "$@"
    else
        # echo "Session $1 already exists and is alive."
        return 0
    fi
}
###
http-static-py () {
    python -m http.server "${1:-8000}"
}
##
export EMACS_SOCKET_NAME="${EMACS_SOCKET_NAME:-${HOME}/tmp/.emacs-servers/server}"

emc-gateway () {
    ALTERNATE_EDITOR="" LOGNAME="$(whoami)" DOOMDIR=~/doom.d pxa emacsclient -t "$@"
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
proxy-available-p () {
  local port="${1:-2096}"
  local host="${2:-localhost}"

  #: Attempt to establish a connection to the proxy
  if nc -z "$host" "$port" >/dev/null 2>&1; then
    # echo "HTTP proxy is running on $host:$port"

    return 0
  else
    # echo "HTTP proxy is not running on $host:$port"

    return 1
  fi
}
##
http-static-caddy () {
    caddy file-server --browse --listen "${2:-0.0.0.0}:${1:-8000}"
}
##
alias rsp-safe='rsync --verbose --checksum --protect-args --human-readable --xattrs --times --info=progress2 --partial-dir=.rsync-partial -r'
# partial-dir supports resume
##
