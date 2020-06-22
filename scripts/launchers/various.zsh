#!/usr/bin/env zsh
tmuxnew serve-dl 'cd ~/Downloads && caddy run' # miniserve -- .' #http-server
#tmuxnew splash 'docker run -it -p 8050:8050 scrapinghub/splash'
borgpath=~/code/betterborg/stdborg.py
tmuxnew julia "zsh -c 'cdd $(gq $borgpath) && python3 $(gq $borgpath)'"
