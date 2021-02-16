#!/usr/bin/env zsh

powersaving_status_del
wireguard_enabled_del

tmuxnew BrishGarden brishgarden

# tmuxnew supercollider scsynth -u 57110 -a 1024 -i 2 -o 2 -R 0 -l 100  -B 127.0.0.1

# needs to run by cron # tmux new -d -s books "$NIGHTDIR/zsh/book-checker.zsh"

###
# tmuxnew v2ray v2ray -config /Users/evar/cellar/notes/private/configs/zii/v2ray/v1.zii.json

tmuxnew naive-zii naive --listen="socks://127.0.0.1:1080" --proxy="https://alice:$NP_PASS1@np.zii.lilf.ir" --log
tmuxnew naive-eva naive --listen="socks://127.0.0.1:1078" --proxy="https://alice:$NP_PASS0@np.lilf.ir" --log

# tmuxnewsh2 trojan trojan -c "$nightNotes"/private/configs/zii/trojan_client.json
# tmuxnew trojan-client-v1-zii trojan -c $nightNotes/private/configs/zii/trojan_client_v1.json

v2-on
# tmuxnewsh2 socks2http hpts --level info -s 127.0.0.1:1080 -p 1087 # https://github.com/oyyd/http-proxy-to-socks
###
tmuxnewsh2 hotwords-porc porc-listen
## These two might not load properly in the cron context
iterm-boot
ot-server-daemon
##
