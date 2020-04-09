#!/usr/bin/env zsh
mkdir -p ~/log
arista='-1001154785017'
water='-1001293952668'
#tmuxnew rss-mol "rss-tsend.zsh 'https://www.royalroad.com/syndication/21220' '.*' Arstar"
tmuxnew rss-stratechery "rss-tsend.zsh 'http://stratechery.com/feed/' '.*' $water"
tmuxnew rss-hn "rt_e=(tl -p 'hn | ') rss-tsend.zsh 'http://hnapp.com/rss?q=score%3E500' '.*' $arista"
tmuxnew rss-novelupdates "rt_e=tlrlu rss-tsend.zsh 'https://www.novelupdates.com/rss.php?uid=145566&unq=5c39d8aba43cc&type=0&lid=local' '.*' $water"
#tmuxnew rss-wuxia "rss-tsend.zsh 'https://www.wuxiaworld.com/feed/chapters' 'Renegade Immortal' Arstar"
