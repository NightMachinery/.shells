#!/usr/bin/env zsh

setopt interactive_comments
tmuxnew () {
    tmux kill-session -t "$1" &> /dev/null
    tmux new -d -s "$@"
}
##
tmuxnew redis redis-server
##
tmuxnew ss sudo ss-server -c /root/ss.json
# @reboot tmux new -d -s ss ss-server -c /root/ss.json
##
tmuxnew v2ray v2ray -config /usr/local/etc/v2ray/config.json
##
tmuxnew caddy-serve caddy run --config /home/walle/Caddyfile
# @reboot tmux new -d -s caddy-serve caddy run --config /home/walle/Caddyfile
##
