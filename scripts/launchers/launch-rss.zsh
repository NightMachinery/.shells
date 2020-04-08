#!/usr/bin/env zsh
mkdir -p ~/log
#tmuxnew rss-mol "rss-tsend.zsh 'https://www.royalroad.com/syndication/21220' '.*' Arstar"
tmuxnew rss-stratechery "rss-tsend.zsh 'http://stratechery.com/feed/' '.*' Arstar"
tmuxnew rss-hn "rss-tsend.zsh 'http://hnapp.com/rss?q=score%3E500' '.*' -1001154785017"
tmuxnew rss-novelupdates "rss-tsend.zsh 'https://www.novelupdates.com/rss.php?uid=145566&unq=5c39d8aba43cc&type=0&lid=local' '.*' Arstar"
#tmuxnew rss-wuxia "rss-tsend.zsh 'https://www.wuxiaworld.com/feed/chapters' 'Renegade Immortal' Arstar"
