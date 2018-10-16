#!/usr/bin/env zsh
gtimeout 6s libgen -s "$*" &> /dev/null
if [ $? -eq 124 ]; then
    terminal-notifier -message "$*" -title "Book Has Become Available!"
    echo "$* is now available!"
fi
