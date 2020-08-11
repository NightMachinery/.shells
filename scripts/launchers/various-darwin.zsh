#!/usr/bin/env zsh

tmuxnew BrishGarden brishgarden
ot-server-daemon

# needs to run by cron # tmux new -d -s books "$NIGHTDIR/zsh/book-checker.zsh"
