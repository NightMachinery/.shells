#!/usr/bin/env zsh
tmux new -d -s rss-mol "rss-tsend.zsh 'https://www.royalroad.com/syndication/21220' '.*' Orphicality"
tmux new -d -s rss-stratechery "rss-tsend.zsh 'http://stratechery.com/feed/' '.*' Orphicality"
tmux new -d -s rss-novelupdates "rss-tsend.zsh 'https://www.novelupdates.com/rss.php?uid=145566&unq=5c39d8aba43cc&type=0&lid=local' '.*' Orphicality"
tmux new -d -s rss-wuxia "rss-tsend.zsh 'https://www.wuxiaworld.com/feed/chapters' 'Renegade Immortal' Orphicality"
