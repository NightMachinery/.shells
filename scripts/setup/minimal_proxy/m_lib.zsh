#!/usr/bin/env zsh
##
setopt pipefail # errexit
##
USER_BIN="$HOME/bin"
mkdir -p "$USER_BIN"

mkdir -p ~/Downloads
##
# print -r -- 'export PATH="${USER_BIN}:$PATH"' >> ~/.zshenv

# touch ~/.privateShell
# print -r -- 'source ~/.privateShell' >> ~/.zshenv
##
pkgs=(
    mosh

    trojan

    libnss3-tools

    pip
)
##
function golang-install {
    wget https://go.dev/dl/go1.18.1.linux-amd64.tar.gz

    sudo rm -rf /usr/local/go && sudo tar -C /usr/local -xzf go1.18.1.linux-amd64.tar.gz

    # print -r -- 'export PATH="$PATH:/usr/local/go/bin"' >> ~/.zshenv
}
##
function install {
    sudo apt-get install -y "$1"
}
##
function xcaddy-install-ubuntu {
    sudo apt install -y debian-keyring debian-archive-keyring apt-transport-https
    curl -1sLf 'https://dl.cloudsmith.io/public/caddy/xcaddy/gpg.key' | sudo tee /etc/apt/trusted.gpg.d/caddy-xcaddy.asc
    curl -1sLf 'https://dl.cloudsmith.io/public/caddy/xcaddy/debian.deb.txt' | sudo tee /etc/apt/sources.list.d/caddy-xcaddy.list
    sudo apt update
    sudo apt install xcaddy
}

function caddy-install-ubuntu {
    #: @broken
    ##
    sudo apt install -y debian-keyring debian-archive-keyring apt-transport-https
    curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' | sudo gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg
    curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' | sudo tee /etc/apt/sources.list.d/caddy-stable.list
    sudo apt update
    sudo apt install -y caddy
}

function caddy-install {
    # xcaddy-install-ubuntu
    go install github.com/caddyserver/xcaddy/cmd/xcaddy@latest || return $?

    cd "${USER_BIN}"
    xcaddy build --with github.com/mholt/caddy-webdav --with github.com/caddyserver/forwardproxy@caddy2=github.com/klzgrad/forwardproxy@naive
    sudo setcap 'cap_net_bind_service=+ep' ./caddy
}
##
