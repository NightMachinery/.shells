#!/usr/bin/env zsh
mkdir -p ~/log
rt_skip="$rt_skip"

##
source-cmd launch-tumblr.zsh
# source-cmd launch-goodreads-authors.zsh
## podcasts
typeset -A podcasts
podcasts['techmeme']='https://rss.art19.com/techmeme-ridehome'
podcasts['Machine Learning Street Talk']='https://anchor.fm/s/1e4a0eac/podcast/rss'

tmuxnewshenv="rt_skip='$rt_skip' rt_e=(@opts dest '$tlg_podcastgen' @ podcast2tel) rt_notel=y rt_ge=( rsstail -i 120 -e -n 3 -N ) rt_eud=0" tmuxnewsh rss-podcastgen rss-tsend ${(@v)podcasts}
## uni
# tmuxnewshenv="rt_skip='$rt_skip' rt_e=(amar-process) rt_notel=y rt_ge=(withchrome getlinks-uniq -e '/session/id/video/') rt_nt=y rt_eid=$((3600*3)) rt_eud=0" tmuxnewsh rss-ocwvids rss-tsend 'http://ocw.sharif.edu/course/id/224'
##
tmuxnewsh2 rss-lw rt_skip=$rt_skip rt_e=rss-tll rss-tsend 'https://www.greaterwrong.com/users/zvi?show=posts&format=rss'

tmuxnewshenv="rt_skip='$rt_skip' rt_eud=1 rt_e=(withchrome rss-tl)" tmuxnewsh \
    rss-gen-withchrome rss-tsend \
    'https://www.bloomberg.com/opinion/authors/ARbTQlRLRjE/matthew-s-levine.rss' \

tmuxnewshenv="rt_skip='$rt_skip' rt_eud=1 rt_e='rss-tl'" tmuxnewsh \
    rss-gen rss-tsend \
    'https://www.scottaaronson.com/blog/?feed=rss2' \
    'https://julialang.org/feed.xml' \
    'https://discourse.julialang.org/u/davidanthoff/activity/topics.rss' \
    'https://feeds2.feedburner.com/GFWReport' \
    'http://www.viridiandreams.net/feed/' \
    'https://www.oreilly.com/radar/topics/radar-trends/feed/index.xml' \
    # 'https://blog.pragmaticengineer.com/rss/' \
    # 'https://www.anandtech.com/rss/' \
    # 'https://www.overcomingbias.com/feed' \

# 'https://danluu.com/atom.xml' \ #: the urls seem to change and thus trigger mass downloads on our part
# 'https://buttondown.email/hillelwayne/rss' 'https://huyenchip.com/feed.xml' 'https://tamaspapp.eu/post/index.xml'
# Rob Henderson Newsletter (Human Nature series) https://www.amazon.com/Incentives-Motivation-Information-Donald-Campbell-dp-1107610338/dp/1107610338/ref=dp_ob_title_bk

tmuxnewshenv="rt_skip='$rt_skip' rt_eud=1 rt_e='rss-tll'" tmuxnewsh rss-gen-tll rss-tsend 'https://medium.com/feed/@superwuster'
# 'https://www.themoneyillusion.com/feed/'

#: Alice does ML: '-1001334871190'
tmuxnewshenv="rt_skip='y' rt_id='-1001334871190' rt_eud=1 rt_e='true'" tmuxnewsh \
    rss-tlg-ml rss-tsend \
    'http://jalammar.github.io/feed.xml' \
    'https://hf.co/blog/feed.xml' \
    'http://distill.pub/rss.xml' \
    'https://explosion.ai/feed' \
    'https://mbernste.github.io/feed.xml' \
    'http://karpathy.github.io/feed.xml' \
    'http://mccormickml.com/atom.xml' \
    'https://lilianweng.github.io/lil-log/feed.xml' \
    'http://www.borealisai.com/en/rss.xml' \
    'https://www.eigenfoo.xyz/feed.xml' \
    'https://medium.com/feed/@jonathan-hui' \
    'https://mchromiak.github.io/feeds/all.atom.xml' \
    'https://ruder.io/rss/index.rss' \
    'https://bmk.sh/atom.xml' \
    'http://googleaiblog.blogspot.com/atom.xml' \
    'https://newsletter.ruder.io/?format=rss' \
    'https://sebastianraschka.com/rss_feed.xml' \
    'https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw' \
    'https://www.youtube.com/feeds/videos.xml?channel_id=UCZHmQk67mSJgfCCTn7xBfew' \
    'https://www.youtube.com/feeds/videos.xml?channel_id=UCZHmQk67mSJgfCCTn7xBfew'
# The Youtube channels are listed on [[id:03eb072e-ca5f-4eda-846b-7d148c124dd7][ML/learn/video]]
##
# tmuxnewshenv="rt_skip='$rt_skip' rt_e=tlrlu" tmuxnewsh rss-genu rss-tsend 'https://www.novelupdates.com/rss.php?uid=145566&unq=5c39d8aba43cc&type=0&lid=local'
##
#tmuxnewshenv="rt_skip='$rt_skip' " tmuxnewsh rss-royalroad rss-tsend 'https://www.royalroad.com/syndication/21322/'

tmuxnewshenv="rt_skip='$rt_skip' rt_e=(tlrl-tmp -p 'stratechery | ')" tmuxnewsh rss-stratechery rss-tsend 'http://stratechery.com/feed/'

tmuxnewshenv="rt_skip='$rt_skip' rt_c=(rss-ctitle) rc_t=(-v 'Ansi Common Lisp') rt_e=(tlrl-tmp -p 'Paul Graham | ')" tmuxnewsh rss-paul rss-tsend 'http://www.aaronsw.com/2002/feeds/pgessays.rss'

## @alt https://www.oreilly.com/radar/topics/radar-trends/feed/index.xml
# tmuxnewshenv="rt_skip='$rt_skip' rt_ge=(getlinks-c -e 'radar-trends-to-watch') rt_nt=y rt_eid=$((3600*24)) rt_eud=0" tmuxnewsh rss-oreillyTrends rss-tsend 'https://www.oreilly.com/radar/topics/radar-trends/'
##

tmuxnewshenv="rt_skip='$rt_skip' rt_e=(tsend-rssln $ephemeral) rt_notel=y rt_id=$arista" tmuxnewsh rss-techmeme rss-tsend 'https://www.techmeme.com/feed.xml'

tmuxnewshenv="rt_skip='$rt_skip' rt_e=true rt_id=$arista rt_duplicates_key=hn rt_eud=0" tmuxnewsh rss-hn rss-tsend 'http://hnapp.com/rss?q=score%3E400'

# tmuxnewshenv="rt_skip='$rt_skip' rt_e=true rt_id=$arista rt_eid=$((3600*3))" tmuxnewsh rss-lobsters rss-tsend 'https://lobste.rs/top/rss'

# tmuxnewshenv="rt_skip='$rt_skip' rt_c=(rss-ctitle) rc_t='Renegade Immortal' " tmuxnewsh rss-wuxia 'https://www.wuxiaworld.com/feed/chapters'
