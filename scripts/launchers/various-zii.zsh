#!/usr/bin/env zsh

borgdir=~/code/betterborg/
tmuxnew julia "dash -c 'cd $(gq $borgdir) && borg_admins=madscientistX $(gq "$(realpath2 python3)") $(gq $borgdir/stdborg.py)'"

tmuxnew wirehole "cd ~/code/misc/wirehole && docker-compose up"
