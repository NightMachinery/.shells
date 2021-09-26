#!/usr/bin/env zsh

tmuxnewsh2 serve-dl caddy run --config ~/Caddyfile

sftpgo-serve-dl 'fall in love'

tmuxnewsh2 vscode SERVICE_URL="https://marketplace.visualstudio.com/_apis/public/gallery" ITEM_URL="https://marketplace.visualstudio.com/items" code-server --bind-addr '127.0.0.1:7451'  --ignore-last-opened # --ignore-last-opened makes vsc open in its default window on each refresh. Use `https://code.zii.lilf.ir/?folder=/home/zii/code/` to open in specific paths.
##
borgdir=~/code/betterborg/
aeirya='90821188'

tmuxnew julia_aeirya "dash -c 'cd $(gq $borgdir) && borg_session=session_aeirya borg_plugin_path=aeirya_plugins borg_admins=${aeirya} $(gq "$(realpath2 python3)") $(gq $borgdir/stdborg.py)'"

tmuxnew julia "dash -c 'cd $(gq $borgdir) && borg_admins=${aeirya} $(gq "$(realpath2 python3)") $(gq $borgdir/stdborg.py)'"

tmuxnew julia_inline "dash -c 'cd $(gq $borgdir) && borg_admins=${aeirya} TELEGRAM_TOKEN=$(gq $TELEGRAM_TOKEN) $(gq "$(realpath2 python3)") $(gq $borgdir/inline.py)'"

tmuxnew wirehole "cd ~/code/misc/wirehole && docker-compose up"

tmuxnew v2ray v2ray -config /usr/local/etc/v2ray/config.json
##
# tmuxnewsh2 trojan indir ~/code/misc/trojan-caddy-docker-compose docker-compose up

# Currently managed by systemd:
# tmuxnew trojan trojan --config /usr/local/etc/trojan/config.json
##
tmuxnew naive-${lilf_user} naive --listen="socks://127.0.0.1:1078" --proxy="https://alice:$NP_PASS0@np.lilf.ir" --log  --concurrency=4
tmuxnewsh2 socks2http hpts --level info -s 127.0.0.1:1078 -p 1088 # https://github.com/oyyd/http-proxy-to-socks
##
tmuxnew znc znc --foreground
##
launch-musicf.zsh
##
chronic-all
