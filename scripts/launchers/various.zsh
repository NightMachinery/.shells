#!/usr/bin/env zsh

until silent redis-cli ping ; do ; sleep 2 ; done
##
# run=create+start+attach
# docker: run --rm to automatically delete it when it exits
# tmuxnewsh2 docker-sourcegraph docker run --name sg --publish 127.0.0.1:7080:7080 --publish 127.0.0.1:3370:3370 --rm --volume ~/.sourcegraph/config:/etc/sourcegraph --volume ~/.sourcegraph/data:/var/opt/sourcegraph sourcegraph/server:3.18.0
##

brishgarden-boot 16 /api/v1

##
# tmuxnew trojan-go trojan-go -config ~/eva_trojan_server.json
##
# sftpgo-serve-dl 'mercy in love'
#: Using SFTP is slow when accessing the server using Iran's internet. FTP and WebDAV don't seem to work with =sftpgo portable=.

tmuxnew serve-dl-webdav-rclone rclone serve webdav ~/Downloads/ --read-only --addr :8000 --user alice --pass 'mercy in love'
##
#: caddy's memory usage sucks, and oom can kill it. We might need to add `retry` to it, but I want things to break noisily for now.

# tmuxnewsh2 serve-dl caddy run --config $NIGHTDIR/launchers/Caddyfile.json
tmuxnewsh2 serve-dl caddy run --config $NIGHTDIR/launchers/Caddyfile
# miniserve -- . #http-server
##
# tmuxnew shadowsocks-ss ss-server -c "$nightNotes/private/configs/eva/shadowsocks/ss.json" # see `man shadowsocks-libev` for config # we might also have this in systemd: `systemctl status ss8324` # needed for the old laptop
# tmuxnew wirehole "cd ~/code/wirehole && docker-compose up"
# tmuxnewsh2 v2-socks v2ray -config $NIGHTDIR/configFiles/v2ray/socks_eva.json

# tmuxnew vless xray -config ~/vless.json
tmuxnew vless-reality xray -config ~/vless_reality_server.json

# tmuxnew gost-ssh gost -L ssh://alice:unfamed-scaum-contagiosity-bimodal-anthranoyl@:8104
# tmuxnew gost-wss gost -L 'http+wss://alice:unfamed-scaum-contagiosity-bimodal-anthranoyl@:8103'

# tmuxnew hysteria hysteria server --config ~/hysteriav2.yaml
##
# tmuxnewsh2 hi10-notify hi10-new-notify 'mahouka|Mushoku Tensei|tokyo revenger|kumo desu ga'
##
#tmuxnew splash 'docker run -it -p 8050:8050 scrapinghub/splash'
##
borgdir=~/code/betterborg/
# tmuxnewsh2 juliaX borg_brish_count=0 python stdborg.py # for testing iterations quickly
tmuxnew julia "dash -c 'cd $(gq $borgdir) && $(gq "$(realpath2 python3)") $(gq $borgdir/start_server.py)'"
tmuxnew julia_papersonegai "dash -c 'cd $(gq $borgdir) && borg_session=session_papersonegai borg_plugin_path=papersonegai_plugins borg_brish_count=1 $(gq "$(realpath2 python3)") $(gq $borgdir/stdborg.py)'"
tmuxnew julia_jlib "dash -c 'cd $(gq $borgdir) && borg_session=session_jlib borg_plugin_path=jlib_plugins borg_brish_count=10 $(gq "$(realpath2 python3)") $(gq $borgdir/stdborg.py)'"
tmuxnew betterborg_stt "dash -c 'cd $(gq $borgdir) && borg_session=session_stt borg_plugin_path=stt_plugins borg_brish_count=1 $(gq "$(realpath2 python3)") $(gq $borgdir/stdborg.py)'"
tmuxnew betterborg_tts "dash -c 'cd $(gq $borgdir) && borg_session=session_tts borg_plugin_path=tts_plugins borg_brish_count=1 $(gq "$(realpath2 python3)") $(gq $borgdir/stdborg.py)'"
tmuxnew betterborg_llm_chat "dash -c 'cd $(gq $borgdir) && GEMINI_SPECIAL_HTTP_PROXY=http://127.0.0.1:2089 borg_session=session_llm_chat borg_plugin_path=llm_chat_plugins borg_brish_count=1 $(gq "$(realpath2 python3)") $(gq $borgdir/stdborg.py)'"
tmuxnew julia_inline "dash -c 'cd $(gq $borgdir) && TELEGRAM_TOKEN=$(gq $TELEGRAM_TOKEN) $(gq "$(realpath2 python3)") $(gq $borgdir/inline.py)'"
##
# @see 'lnc-epub' for non-interactive usage
tmuxnewsh2 lightnovel-crawler PUBLIC_DATA_PATH=$HOME/Downloads/tmp PUBLIC_ADDRESS=https://files.lilf.ir/tmp/  BOT=telegram TELEGRAM_TOKEN=$TELEGRAM_TOKEN_OCEAN lightnovel-crawler
##
# if test -e ~/code/kotlin/smsq/go/config.json ; then
#     tmuxnew smsq smsq ~/code/kotlin/smsq/go/config.json
# fi
##
# tmuxnewsh2 notes-hugo indir ~cod/hugo/notes-hugo hugo --bind="127.0.0.1" --baseURL="https://notes.lilf.ir" --appendPort=false --gc --port 1313 serve
##
tmuxnewsh2 deluged deluged -d
tmuxnewsh2 deluge-web deluge-web -d --interface='127.0.0.1' --port='8666' --loglevel=info
##
tmuxnew email-aggregator-bot zsh -c "API_TOKEN=${emailaggbot_api_token} DATABASE_NAME=~/mail_gather_database.db mail-gather-bot"
##
tmuxnew feed_updater1 env TSEND_TOKEN="${TSEND_TOKEN}" TSEND_BACKEND="${TSEND_BACKEND}" "${commands[python]}" "${commands[feed_updater1.py]}"
##
# launch-musicf.zsh
##
chronic-all
