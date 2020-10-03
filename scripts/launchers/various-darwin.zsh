#!/usr/bin/env zsh

tmuxnew BrishGarden brishgarden

tmuxnew v2ray v2ray -config /Users/evar/cellar/notes/private/configs/zii/v2ray/v1.zii.json

## These two might not load properly in the cron context
iterm-boot
ot-server-daemon
##

# needs to run by cron # tmux new -d -s books "$NIGHTDIR/zsh/book-checker.zsh"
