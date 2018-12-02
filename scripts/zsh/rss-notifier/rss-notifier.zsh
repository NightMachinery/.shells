#!/usr/bin/env zsh
rsstail -l -u "$1" -n 0 | while read -r line1
do
    read -r line2
    if ggrep -P --silent "$2" <<< "$line1" ; then
        # Use sth like parallel  --null -N 2 tsend me \"{1} $'\n' {2}\" or ensure-run.zsh "60s" "parallel  --null -N 2 tsend me \\\"{1} $'\n' {2}\\\""
        # local c="printf '%b' $line1'\0'$line2|${@:3}"
        # echo "$c"
        # eval "$c"
        printf '%b' "$line1"'\0'"$line2"'\0'
        # echo
    fi
done
