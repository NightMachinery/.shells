#!/usr/bin/env zsh
tmuxnewsh2 docker-sourcegraph docker run --publish 127.0.0.1:7080:7080 --publish 127.0.0.1:3370:3370 --rm --volume ~/.sourcegraph/config:/etc/sourcegraph --volume ~/.sourcegraph/data:/var/opt/sourcegraph sourcegraph/server:3.18.0
tmuxnew serve-dl 'cd ~/Downloads && caddy run' # miniserve -- .' #http-server
#tmuxnew splash 'docker run -it -p 8050:8050 scrapinghub/splash'
borgpath=~/code/betterborg/stdborg.py
tmuxnew julia "zsh -c 'cdd $(gq $borgpath) && python3 $(gq $borgpath)'"
