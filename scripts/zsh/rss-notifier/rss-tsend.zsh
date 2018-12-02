#!/usr/bin/env zsh
rss-notifier.zsh "$1" "$2"|tee ~/log/rss-tsend.log |while read -d "" -r t; do
    read -d "" -r l
    ensure-run.zsh "150s" tsend "${3:-me}" "$t
$l"
done
echo restarting rss-tsend for "$2" >> ~/log/rss-tsend.log
exec rss-tsend.zsh "$1" "$2" "$3"
