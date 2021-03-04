#!/usr/bin/env zsh

powersaving_status_del
# wireguard_enabled_del
##
darwin-dns-set 127.0.0.1 1.1.1.1
##

tmuxnew BrishGarden brishgarden

# tmuxnew supercollider scsynth -u 57110 -a 1024 -i 2 -o 2 -R 0 -l 100  -B 127.0.0.1

# needs to run by cron # tmux new -d -s books "$NIGHTDIR/zshlang/book-checker.zsh"

###
# tmuxnew v2ray v2ray -config /Users/evar/cellar/notes/private/configs/zii/v2ray/v1.zii.json

# orig 1080
tmuxnew naive-zii naive --listen="socks://127.0.0.1:1080" --proxy="https://alice:$NP_PASS1@np.zii.lilf.ir" --log  --concurrency=4

tmuxnew naive-eva naive --listen="socks://127.0.0.1:1078" --proxy="https://alice:$NP_PASS0@np.lilf.ir" --log  --concurrency=4

# tmuxnewsh2 trojan trojan -c "$nightNotes"/private/configs/zii/trojan_client.json
# tmuxnew trojan-client-v1-zii trojan -c $nightNotes/private/configs/zii/trojan_client_v1.json

v2-on # v2ray-genrouter
# tmuxnewsh2 socks2http hpts --level info -s 127.0.0.1:1081 -p 1087 # https://github.com/oyyd/http-proxy-to-socks
###
# tmuxnewsh2 hotwords-porc porc-listen
##
tmuxnewsh2 browser-recordings-process-watch browser-recordings-process-watch
## These two might not load properly in the cron context
iterm-boot
ot-server-daemon
##
