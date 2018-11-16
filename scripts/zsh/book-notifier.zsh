#!/usr/bin/env zsh
export PYTHONIOENCODING=utf8
export LANG="en_US.UTF-8"
if ! is-online &> /dev/null ; then
    echo Not online
    exit 0
fi
gtimeout 10s libgen -s "$*" &> /dev/null
local res=$?
# echo $res
if [ $res -eq 124 ]; then
    terminal-notifier -message "$*" -title "Book Has Become Available!"
    echo "$* is now available!"
fi
