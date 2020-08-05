#!/usr/bin/env zsh

##
# run=create+start+attach
# docker: run --rm to automatically delete it when it exits
# tmuxnewsh2 docker-sourcegraph docker run --name sg --publish 127.0.0.1:7080:7080 --publish 127.0.0.1:3370:3370 --rm --volume ~/.sourcegraph/config:/etc/sourcegraph --volume ~/.sourcegraph/data:/var/opt/sourcegraph sourcegraph/server:3.18.0
##

tmuxnew BrishGarden brishgarden /api/v1

##
# caddy's memory usage sucks, and oom can kill it. We might need to add `retry` to it, but I want things to break noisily for now.
tmuxnew serve-dl 'caddy run --config $NIGHTDIR/launchers/Caddyfile' # miniserve -- .' #http-server
##

#tmuxnew splash 'docker run -it -p 8050:8050 scrapinghub/splash'
borgpath=~/code/betterborg/stdborg.py
tmuxnew julia "zsh -c 'cdd $(gq $borgpath) && python3 $(gq $borgpath)'"
