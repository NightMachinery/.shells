#!/usr/bin/env zsh

tmuxnewsh2 serve-dl caddy run --config ~/Caddyfile

borgdir=~/code/betterborg/
tmuxnew julia "dash -c 'cd $(gq $borgdir) && borg_admins=madscientistX $(gq "$(realpath2 python3)") $(gq $borgdir/stdborg.py)'"
tmuxnew julia_inline "dash -c 'cd $(gq $borgdir) && borg_admins=madscientistX TELEGRAM_TOKEN=$(gq $TELEGRAM_TOKEN) $(gq "$(realpath2 python3)") $(gq $borgdir/inline.py)'"

tmuxnew wirehole "cd ~/code/misc/wirehole && docker-compose up"

tmuxnew v2ray v2ray -config /usr/local/etc/v2ray/config.json

##
# tmuxnewsh2 trojan indir ~/code/misc/trojan-caddy-docker-compose docker-compose up

# Currently managed by systemd:
# tmuxnew trojan trojan --config /usr/local/etc/trojan/config.json
##
