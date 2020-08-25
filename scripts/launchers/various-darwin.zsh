#!/usr/bin/env zsh

tmuxnew BrishGarden brishgarden

## These two might not load properly in the cron context
iterm-boot
ot-server-daemon
##

# needs to run by cron # tmux new -d -s books "$NIGHTDIR/zsh/book-checker.zsh"
