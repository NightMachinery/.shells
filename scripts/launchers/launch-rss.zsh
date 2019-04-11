#!/usr/bin/env zsh
tmux new -d -s rss-stratechery "rss-tsend.zsh 'http://stratechery.com/feed/' '*' Orphicality"
tmux new -d -s rss-lnbastion "rss-tsend.zsh 'https://www.lightnovelbastion.com/rss.php' 'Death Mage' Orphicality"
tmux new -d -s rss-wuxia "rss-tsend.zsh 'https://www.wuxiaworld.com/feed/chapters' 'Renegade Immortal' Orphicality"
