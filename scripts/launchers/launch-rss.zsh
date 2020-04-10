#!/usr/bin/env zsh
mkdir -p ~/log
arista='-1001154785017'
water='-1001293952668'
#tmuxnew rss-mol "rss-tsend.zsh 'https://www.royalroad.com/syndication/21220' '.*' Arstar"
tmuxnewsh rss-stratechery rss-tsend 'http://stratechery.com/feed/' '.*' $water
#tmuxnewshenv="rt_e=(tl -p 'hn | ')"
tmuxnewshenv='rt_e=true' tmuxnewsh rss-hn rss-tsend 'http://hnapp.com/rss?q=score%3E500' '.*' $arista
tmuxnewshenv='rt_e=tlrlu' tmuxnewsh rss-novelupdates rss-tsend 'https://www.novelupdates.com/rss.php?uid=145566&unq=5c39d8aba43cc&type=0&lid=local' '.*' $water
#tmuxnew rss-wuxia "rss-tsend.zsh 'https://www.wuxiaworld.com/feed/chapters' 'Renegade Immortal' Arstar"
