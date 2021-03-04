#!/usr/bin/env zsh
# Alt: You might want to use ZAlerts.

export PYTHONIOENCODING=utf8
export LANG="en_US.UTF-8"
if ! is-online &> /dev/null ; then
    echo Not online
    exit 0
fi
(gtimeout 20s libgen -s "$*")|grep --silent 'Choose book by ID:'
local res=$?
# echo $res
if [ $res -eq 0 ]; then
    terminal-notifier -message "$*" -title "Book Has Become Available!"
    echo "$* is now available!"
fi
