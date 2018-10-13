#!/usr/bin/env zsh
gtimeout 6s libgen -s "$1" &> /dev/null
if [ $? -eq 124 ]; then
    terminal-notifier -message "$1" -title "Book Has Become Available!"
    echo "$1 is now available!"
fi
