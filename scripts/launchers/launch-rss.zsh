#!/usr/bin/env zsh
mkdir -p ~/log
arista='-1001154785017'
water='-1001293952668'
#tmuxnew rss-mol "rss-tsend.zsh 'https://www.royalroad.com/syndication/21220' '.*' Arstar"
tmuxnewshenv="rt_e=(tl -p 'stratechery | ')" tmuxnewsh rss-stratechery rss-tsend 'http://stratechery.com/feed/'
tmuxnewshenv="rt_e=(tl -p 'Paul Graham | ')" tmuxnewsh rss-paul rss-tsend 'http://www.aaronsw.com/2002/feeds/pgessays.rss'
tmuxnewsh rss-royalroad rss-tsend 'https://www.royalroad.com/syndication/21322/'
tmuxnewshenv="rt_e=true rt_id=$arista" tmuxnewsh rss-hn rss-tsend 'http://hnapp.com/rss?q=score%3E500' 'https://lobste.rs/top/rss'
tmuxnewshenv='rt_e=tlrlu' tmuxnewsh rss-novelupdates rss-tsend 'https://www.novelupdates.com/rss.php?uid=145566&unq=5c39d8aba43cc&type=0&lid=local'
# tmuxnewshenv="rt_c=(rss-ctitle) rc_t='Renegade Immortal' " tmuxnewsh rss-wuxia 'https://www.wuxiaworld.com/feed/chapters'
