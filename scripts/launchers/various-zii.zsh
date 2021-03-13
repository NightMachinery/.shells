#!/usr/bin/env zsh

tmuxnewsh2 serve-dl caddy run --config ~/Caddyfile

tmuxnewsh2 vscode SERVICE_URL="https://marketplace.visualstudio.com/_apis/public/gallery" ITEM_URL="https://marketplace.visualstudio.com/items" code-server --bind-addr '127.0.0.1:7451'  --ignore-last-opened # --ignore-last-opened makes vsc open in its default window on each refresh. Use `https://code.zii.lilf.ir/?folder=/home/zii/code/` to open in specific paths.
##
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
typeset -A artists
####
artists['Mohsen Namjoo']='https://open.spotify.com/artist/4eVyI1yiHoRjVrxt5y7gGL'
artists['Mohsen Chavoshi']='https://open.spotify.com/artist/2IYl0pVatTl6O2d1CQj6GQ'
artists['Reza Yazdani']='https://open.spotify.com/artist/0tctijgZruddzQ8FCgvDj2'
###
artists['Marjan Farsad']='https://open.spotify.com/artist/5sGy7zfcUWYPh3wHbyz4fn'
artists['Hani Niroo']='https://open.spotify.com/artist/07xBlQ8c7xhTa7IefYshum'
artists['Sogand']='https://open.spotify.com/artist/2mGkJsyXC8byI83UHSO4w1'
artists['Rana Farhan']='https://open.spotify.com/artist/5Fq4u0e9OPI4k4R3QC6OZO'
##
artists['Niaz Nawab']='https://open.spotify.com/artist/4hprwICrkEC8ule8JvCcJS'
artists['Aida Shahghasemi']='https://open.spotify.com/artist/1bQlJQRMtgj4p2gy9nN9Mz'
artists['Tara Tiba']='https://open.spotify.com/artist/0s6fPszrilIZ2kmduBE9N7'
artists['Rana Mansour']='https://open.spotify.com/artist/3zrjEdzGRRwrLHv3KQDDuB'
####
artists['Alexander Rybak']='https://open.spotify.com/artist/3LLNDXrxL4uxXtnUJS5XWM'
artists['Pharrell Williams']='https://open.spotify.com/artist/3V38zs2XJF5nXDv5jt31zm'
artists['Depeche Mode']='https://open.spotify.com/artist/762310PdDnwsDxAQxzQkfX'
artists['System Of A Down']='https://open.spotify.com/artist/5eAWCfyUhZtHHtBdNk56l1'
artists['Maroon 5']='https://open.spotify.com/artist/04gDigrS5kc9YWfZHwBETP'
artists['Owl City']='https://open.spotify.com/artist/07QEuhtrNmmZ0zEcqE9SF6'
##
artists['Brian Eno']='https://open.spotify.com/artist/7MSUfLeTdDEoZiJPDSBXgi'
###
# artists['Becky G']='https://open.spotify.com/artist/4obzFoKoKRHIphyHzJ35G3'
artists['Halsey']='https://open.spotify.com/artist/26VFTg2z8YR0cCuwLzESi2'
artists['AURORA']='https://open.spotify.com/artist/1WgXqy2Dd70QQOU7Ay074N'
artists['Selena Gomez']='https://open.spotify.com/artist/0C8ZW7ezQVs4URX5aX7Kqx'
artists['Demi Lovato']='https://open.spotify.com/artist/6S2OmqARrzebs0tKUEyXyp'
artists['Billie Eilish']='https://open.spotify.com/artist/6qqNVTkY8uBg9cP3Jd7DAH'
artists['Lily Allen']='https://open.spotify.com/artist/13saZpZnCDWOI9D4IJhp1f'
artists['Lana Del Rey']='https://open.spotify.com/artist/00FQb4jTyendYWaN8pK0wa'
artists['Ariana Grande']='https://open.spotify.com/artist/66CXWjxzNUsdJxJ2JdwvnR'
# artists['INNA']='https://open.spotify.com/artist/2w9zwq3AktTeYYMuhMjju8'
artists['Lorde']='https://open.spotify.com/artist/163tK9Wjr9P9DmM0AVK7lm'
artists['Melanie Martinez']='https://open.spotify.com/artist/63yrD80RY3RNEM2YDpUpO8'
# artists['Taylor Swift']='https://open.spotify.com/artist/06HL4z0CvFAxyc27GXpf02'
artists['Dido']='https://open.spotify.com/artist/2mpeljBig2IXLXRAFO9AAs'
artists['Carly Rae Jepsen']='https://open.spotify.com/artist/6sFIWsNpZYqfjUpaCgueju'
##
artists['Hey Violet']='https://open.spotify.com/artist/4JNfz6aO9ZFz0gp5GY88am'
artists['Cavetown']='https://open.spotify.com/artist/2hR4h1Cao2ueuI7Cx9c7V8'
artists['Camila Cabello']='https://open.spotify.com/artist/4nDoRrQiYLoBzwC5BhVJzF'
artists['Fifth Harmony']='https://open.spotify.com/artist/1l8Fu6IkuTP0U5QetQJ5Xt'
##
# artists['']=''
tmuxnewshenv="rt_skip='$rt_skip' rt_e=(rss-engine-spotify) rt_notel=y rt_nt=y rt_ge=(spotify-discography-get) rt_eud=0 rt_eid=$((3600*24*14))" tmuxnewsh rss-spotify rss-tsend ${(@v)artists}
##
chronic-all
