#!/usr/bin/env zsh
mkdir -p ~/log
arista='-1001154785017'
water='-1001293952668'
rt_skip="$rt_skip"

#tmuxnewshenv="rt_skip='$rt_skip' " tmuxnewsh rss-royalroad rss-tsend 'https://www.royalroad.com/syndication/21322/'

tmuxnewshenv="rt_skip='$rt_skip' rt_e=(tl -p 'stratechery | ')" tmuxnewsh rss-stratechery rss-tsend 'http://stratechery.com/feed/'
# Create tlrl-host that adds the hostname of the URL to the title?
tmuxnewshenv="rt_skip='$rt_skip' rt_eud=1" tmuxnewsh rss-gen rss-tsend 'https://www.themoneyillusion.com/feed/' 'https://www.scottaaronson.com/blog/?feed=rss2'
tmuxnewshenv="rt_skip='$rt_skip' rt_c=(rss-ctitle) rc_t=(-v 'Ansi Common Lisp') rt_e=(tl -p 'Paul Graham | ')" tmuxnewsh rss-paul rss-tsend 'http://www.aaronsw.com/2002/feeds/pgessays.rss'

# alt: https://www.oreilly.com/radar/topics/radar-trends/feed/index.xml
tmuxnewshenv="rt_skip='$rt_skip' rt_ge=(getlinks-c -e 'radar-trends-to-watch') rt_nt=y rt_eid=$((3600*24)) rt_eud=0" tmuxnewsh rss-oreillyTrends rss-tsend 'https://www.oreilly.com/radar/topics/radar-trends/'


tmuxnewshenv="rt_skip='$rt_skip' rt_e=(tsend-rssln $arista) rt_notel=y rt_id=$arista" tmuxnewsh rss-techmeme rss-tsend 'https://www.techmeme.com/feed.xml'
tmuxnewshenv="rt_skip='$rt_skip' rt_e=true rt_id=$arista" tmuxnewsh rss-hn rss-tsend 'http://hnapp.com/rss?q=score%3E500'
tmuxnewshenv="rt_skip='$rt_skip' rt_e=true rt_id=$arista rt_eid=$((3600*3))" tmuxnewsh rss-lobsters rss-tsend 'https://lobste.rs/top/rss'

tmuxnewshenv="rt_skip='$rt_skip' rt_e=tlrlu" tmuxnewsh rss-novelupdates rss-tsend 'https://www.novelupdates.com/rss.php?uid=145566&unq=5c39d8aba43cc&type=0&lid=local'
# tmuxnewshenv="rt_skip='$rt_skip' rt_c=(rss-ctitle) rc_t='Renegade Immortal' " tmuxnewsh rss-wuxia 'https://www.wuxiaworld.com/feed/chapters'
