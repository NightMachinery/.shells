#!/usr/bin/env zsh
# DEPRECATED use rss-tsend

local engine=("${rt_e[@]:-tl}")
rss-notifier.zsh "$1" "$2"|& tee ~/log/rss-tsend.log |while read -d "" -r t; do
    read -d "" -r l
    ec "$t
    $1
    "
    ensure-run.zsh "150s" tsend -- "${3:-me}" "$t
$l"
    sleep 120 #because wuxia sometimes sends unupdated pages
    reval "$engine[@]" "$l" "$t"
done
echo restarting rss-tsend for "$2" >> ~/log/rss-tsend.log
exec rss-tsend.zsh "$1" "$2" "$3"
