#!/usr/bin/env zsh

tmuxnew BrishGarden brishgarden

tmuxnew v2ray v2ray -config /Users/evar/cellar/notes/private/configs/zii/v2ray/v1.zii.json

## These two might not load properly in the cron context
iterm-boot
ot-server-daemon
##

# needs to run by cron # tmux new -d -s books "$NIGHTDIR/zsh/book-checker.zsh"

tmuxnewsh2 trojan trojan -c "$nightNotes"/private/configs/zii/trojan_client.json

tmuxnewsh2 socks2http hpts --level info -s 127.0.0.1:1080 -p 1087 # https://github.com/oyyd/http-proxy-to-socks
