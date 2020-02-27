#!/usr/bin/env zsh
tmux new -d -s serve-dl 'cd ~/Downloads && http-server'
tmux new -d -s splash 'docker run -it -p 8050:8050 scrapinghub/splash'
borgpath=~/code/betterborg/stdborg.py
tmux new -d -s julia "zsh -c 'cdd $(gq $borgpath) && python3 $(gq $borgpath)'"
