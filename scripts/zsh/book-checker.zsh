#!/usr/bin/env zsh
# Use `tmux new -d -s name 'this script'` in cron.
zmodload zsh/mapfile
local blist=${blist:-~/.wanted-books}
local books=( "${(f)mapfile[$blist]}" )
for i in $books
{
    if ! ggrep --silent -P '^#.*' <<<"$i"; then
        echo "$i"
        "$NIGHTDIR"/zsh/book-notifier.zsh "$i"
    fi
}
