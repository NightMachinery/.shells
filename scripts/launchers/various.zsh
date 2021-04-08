#!/usr/bin/env zsh

##
# run=create+start+attach
# docker: run --rm to automatically delete it when it exits
# tmuxnewsh2 docker-sourcegraph docker run --name sg --publish 127.0.0.1:7080:7080 --publish 127.0.0.1:3370:3370 --rm --volume ~/.sourcegraph/config:/etc/sourcegraph --volume ~/.sourcegraph/data:/var/opt/sourcegraph sourcegraph/server:3.18.0
##

brishgarden-boot 16 /api/v1

##
# caddy's memory usage sucks, and oom can kill it. We might need to add `retry` to it, but I want things to break noisily for now.
tmuxnewsh2 serve-dl caddy run --config $NIGHTDIR/launchers/Caddyfile # miniserve -- . #http-server

tmuxnew shadowsocks-ss ss-server -c "$nightNotes/private/configs/eva/shadowsocks/ss.json" # see `man shadowsocks-libev` for config # we might also have this in systemd: `systemctl status ss8324` # needed for the old laptop
tmuxnew wirehole "cd ~/code/wirehole && docker-compose up"
# tmuxnewsh2 v2-socks v2ray -config $NIGHTDIR/configFiles/v2ray/socks_eva.json
##
#tmuxnewsh2 mahouka hi10-new-notify mahouka
##
#tmuxnew splash 'docker run -it -p 8050:8050 scrapinghub/splash'
##
borgdir=~/code/betterborg/
# tmuxnewsh2 juliaX borg_brish_count=0 python stdborg.py # for testing iterations quickly
tmuxnew julia "dash -c 'cd $(gq $borgdir) && $(gq "$(realpath2 python3)") $(gq $borgdir/start_server.py)'"
tmuxnew julia_jlib "dash -c 'cd $(gq $borgdir) && borg_session=session_jlib borg_plugin_path=jlib_plugins borg_brish_count=10 $(gq "$(realpath2 python3)") $(gq $borgdir/stdborg.py)'"
tmuxnew julia_inline "dash -c 'cd $(gq $borgdir) && TELEGRAM_TOKEN=$(gq $TELEGRAM_TOKEN) $(gq "$(realpath2 python3)") $(gq $borgdir/inline.py)'"
##
tmuxnewsh2 lightnovel-crawler PUBLIC_DATA_PATH=$HOME/Downloads/tmp PUBLIC_ADDRESS=https://files.lilf.ir/tmp/  BOT=telegram TELEGRAM_TOKEN=$TELEGRAM_TOKEN_OCEAN lightnovel-crawler
##
if test -e ~/code/kotlin/smsq/go/config.json ; then
    tmuxnew smsq smsq ~/code/kotlin/smsq/go/config.json
fi
##
chronic-all
