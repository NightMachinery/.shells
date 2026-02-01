##
#: Minimal proxy is supposed to always be running on a remote system, so we can hardcode the following:
isLocal () {
    return 1
}

isRemote () {
    ! isLocal
}
##
isBash () {
    [[ -n $BASH_VERSION ]]
}

isZsh () {
    [[ -n $ZSH_VERSION ]]
}
##
aliasfn () {
    local name="$1"
    local body="$@[2,-1]"

    if isZsh ; then
        functions[$name]="$body "'"$@"'
    else
        alias "$name"="$body"
    fi
}
##
if infocmp xterm-kitty > /dev/null 2>&1; then
  export TERM="xterm-kitty"
else
  # echo "The terminfo entry for xterm-kitty does not exist." >&2

  export TERM="xterm-256color"
fi
# export TERM="xterm-kitty"
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

##
#: @duplicateCode/fc1c8d786f258e551cae31f17f050814
typeset -g H_FZF_DEFAULT_OPTS="--bind 'shift-up:toggle+up,shift-down:toggle+down,alt-k:toggle+up,alt-j:toggle+down,alt-h:backward-word,alt-l:forward-word,alt-up:preview-up,alt-down:preview-down,alt-n:next-history,alt-p:previous-history,tab:toggle,shift-tab:toggle+beginning-of-line+kill-line,alt-/:toggle-preview,ctrl-l:toggle+beginning-of-line+kill-line,ctrl-t:top,ctrl-s:select-all,alt-enter:print-query,shift-right:replace-query' --multi --hscroll-off 99999 --exact"
#: ctrl-j, ctrl-k: scroll

export FZF_DEFAULT_OPTS="${H_FZF_DEFAULT_OPTS}"
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
psource ~/.private.pre.shared.sh

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

export PATH="${HOME}/.local/share/junest/bin:$PATH"
export PATH="$PATH:${HOME}/.junest/usr/bin_wrappers"
export JUNEST_HOME="${HOME}/.junest"
if test -n "${JUNEST_ENV}" ; then
    #: We are already inside junest, so just use the normal sudo.
    alias sudo-junest=sudo
else
    alias sudo-junest="${HOME}/.junest/usr/bin_wrappers/sudo"
fi
##
mamba_path="${HOME}/miniforge3"
mamba_default_env=""
if test -d "${mamba_path}" ; then
    #: Mamba is installed
    ##
    export PATH="${mamba_path}/bin:${PATH}"
    export MAMBA_ROOT_PREFIX="${mamba_path}"
    micromamba() {
        mamba "$@"
    }
else
    export PATH="${HOME}/anaconda/bin:${PATH}"
    export PATH="${HOME}/miniconda3/bin:${PATH}"
    export MAMBA_ROOT_PREFIX="${HOME}/micromamba"

    mamba_default_env=p310
fi

if isBash ; then
    if eval "$(micromamba shell hook --shell bash)" ; then
        micromamba activate ${mamba_default_env}
    fi
else
    if eval "$(micromamba shell hook --shell zsh)" ; then
        micromamba activate ${mamba_default_env}
    fi
fi
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
alias gf='git fetch'
alias gl='git pull'
alias gp='git push'
alias grv='git remote -v'
alias gss='git status'
alias gd='git diff'
alias gcm='\noglob h-gcm'
h-gcm () {
    git commit -m "$*"
}

glola () {
    LESS=$LESSMIN git log --graph --pretty=format:'%Cred%h%Creset %C(yellow)%ad%Creset %Cgreen(%cr)%Creset %s %C(yellow)%d%Creset %C(bold blue)<%an>%Creset' --date=short --abbrev-commit --all
}

gap () {
  #: @duplicateCode/e7fe1579f27b25514799a50b50f2eff8
  ##
  reval-ec git add -p "$@"

  reval-ec git status --short --untracked-files
}
##
reval() {
    eval "$(gquote "$@")"
}
revaldbg() {
    reval "$@"
}
reval-ec() {
    reval "$@"
}
reval-ecgray() {
    reval "$@"
}
##
proxy-env-unset () { #: proxy unexport
    unset ALL_PROXY all_proxy http_proxy https_proxy HTTP_PROXY HTTPS_PROXY
}

alias pxa87='ALL_PROXY=http://127.0.0.1:1087 all_proxy=http://127.0.0.1:1087 http_proxy=http://127.0.0.1:1087 https_proxy=http://127.0.0.1:1087 HTTP_PROXY=http://127.0.0.1:1087 HTTPS_PROXY=http://127.0.0.1:1087'
alias pxa2096='ALL_PROXY=http://127.0.0.1:2096 all_proxy=http://127.0.0.1:2096 http_proxy=http://127.0.0.1:2096 https_proxy=http://127.0.0.1:2096 HTTP_PROXY=http://127.0.0.1:2096 HTTPS_PROXY=http://127.0.0.1:2096'
alias pxa2097='ALL_PROXY=http://127.0.0.1:2097 all_proxy=http://127.0.0.1:2097 http_proxy=http://127.0.0.1:2097 https_proxy=http://127.0.0.1:2097 HTTP_PROXY=http://127.0.0.1:2097 HTTPS_PROXY=http://127.0.0.1:2097'
alias pxa4096='ALL_PROXY=http://127.0.0.1:4096 all_proxy=http://127.0.0.1:4096 http_proxy=http://127.0.0.1:4096 https_proxy=http://127.0.0.1:4096 HTTP_PROXY=http://127.0.0.1:4096 HTTPS_PROXY=http://127.0.0.1:4096'
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
    TERM=xterm-kitty ALTERNATE_EDITOR="" LOGNAME="$(whoami)" DOOMDIR=~/doom.d pxa emacsclient -t "$@"
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
httpify-socks5 () {
    local socks="${1}"
    local http="${2}"

    tmuxnew "httpify-socks-${socks}-to-${http}" gost -F "socks5://127.0.0.1:${socks}" -L "http://127.0.0.1:${http}"
}
##
http-static-caddy () {
    caddy file-server --browse --listen "${2:-0.0.0.0}:${1:-8000}"
}
##
alias rsp-safe='rsync --verbose --checksum --protect-args --human-readable --xattrs --times --info=progress2 --partial-dir=.rsync-partial -r'
# partial-dir supports resume
##
alias icat='tpix'
alias ic=icat
##
alias trs='rip --'
##
alias ggrep=grep
##
##
typeset -g sharif_net_url_normal='https://net2.sharif.edu'
typeset -g sharif_net_url_ip='https://172.17.1.214'
# typeset -g sharif_net_url_ip='https://198.18.0.8'
typeset -g sharif_net_url="${sharif_net_url_ip}"

function with-sharif-net-url-ip {
    sharif_net_url="${sharif_net_url_ip}" reval-env "$@"
}

function h-sharif-net-curl {
    (
        ecgray "$0: disabled proxies  locally (by running proxy-env-unset in a subshell)"
        proxy-env-unset

        revaldbg curl --insecure "$@"
    )
}

function sharif-net-status {
    h-sharif-net-curl -s "${sharif_net_url}/status" | ggrep -oP '<td(?:\s[^>]*)?>\K.*?(?=</td>)'
}

function sharif-net-login {
    h-sharif-net-curl -d "username=${sharif_vpn_username}&password=${sharif_vpn_passowrd}" -X POST "${sharif_net_url}/login" > /dev/null
    sharif-net-status
}

function sharif-net-logout {
    h-sharif-net-curl -d "" -X POST "${sharif_net_url}/logout"
    sharif-net-status
}
##
function date-tehran {
    TZ='Asia/Tehran' date +'%Y-%m-%d %A %H:%M:%S'
}
##
#: @duplicateCode/b5bd9b0856024ac4d9a4c454de6be2a4
function git-clean-p {
  git diff-index --quiet HEAD --
  #: --quiet  Disable all output of the program. Implies --exit-code.
  #: --exit-code  Make the program exit with codes similar to diff(1). That is, it  exits with 1 if there were differences and 0 means no differences.
}

function git-dirty-p {
  ! git-clean-p
}
##
function path-rm {
    #: @duplicateCode/5a7a8cf2bca9b3e2eff69a63ad963603
    #: actually not duplicated yet
    ##
    # Check if at least one directory is provided as an argument
    if [ "$#" -eq 0 ]; then
        echo "Usage: path-rm <dir1> [dir2 ...]" >&2
        return 1
    fi

    # Assign all input arguments to a local array for directories to remove
    local dirs_to_remove=("$@")
    local newPATH=""
    local path_array=()

    # Determine the current shell: zsh or bash
    if [ -n "$ZSH_VERSION" ]; then
        # -----------------------
        # zsh-Specific Handling
        # -----------------------
        # Split PATH into an array using zsh's parameter expansion
        path_array=("${(@s/:/)PATH}")

    elif [ -n "$BASH_VERSION" ]; then
        # ------------------------
        # bash-Specific Handling
        # ------------------------
        # Save the original Internal Field Separator (IFS)
        local OLDIFS="$IFS"

        # IFS (Internal Field Separator): Temporarily set to =:= to split the =PATH= variable into individual directories.
        IFS=':'

        # Split PATH into an array based on IFS
        path_array=($PATH)

        # Restore the original IFS
        IFS="$OLDIFS"

    else
        # ------------------------
        # Unsupported Shell
        # ------------------------
        echo "Error: Unsupported shell. This function works with bash and zsh only." >&2
        return 1
    fi

    # ------------------------
    # Common Handling
    # ------------------------
    # Iterate over each directory in the path_array
    for dir in "${path_array[@]}"; do
        # Initialize a flag to determine if the current dir should be removed
        local remove=0

        # Check if the current directory matches any of the directories to remove
        for to_remove in "${dirs_to_remove[@]}"; do
            if test -z "$dir" || [ "$dir" = "$to_remove" ]; then
                remove=1
                break
            fi
        done

        # If the directory is not marked for removal, add it to newPATH
        if [ "$remove" -eq 0 ]; then
            if [ -z "$newPATH" ]; then
                newPATH="$dir"
            else
                newPATH="$newPATH:$dir"
            fi
        fi
    done

    # Update the PATH variable in the current shell session
    export PATH="$newPATH"
}
##
myip-ipinfo () {
    curl -H "Authorization: Bearer ${ipinfo_api_token}" "ipinfo.io"
}
alias ci='myip-ipinfo'
##
