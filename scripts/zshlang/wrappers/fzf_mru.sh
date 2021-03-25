#!/bin/dash

echo() {
    printf -- "%s\n" "$*"
}

mru_count=30
if test -z "$context" ; then
    echo "$0: You need to provide a context" >&2
    exit 1
fi


conf_dir="$HOME/.cache/fzf_mru"
mkdir -p "$conf_dir"
conf_cache="$conf_dir/${context}"
touch "$conf_cache"
conf_tmp="${conf_cache}.tmp"

# Removing duplicate lines: seen is an associative-array that Awk will pass every line of the file to. If a line isn't in the array then seen[$0] will evaluate to false. (The name 'seen' does not matter.)
result="$(cat "$conf_cache" - | awk '!seen[$0]++' | fzf --tiebreak=index "$@")" || return $?
if test -n "$result" ; then
    echo "$result" | cat - "$conf_cache" | awk '!seen[$0]++' | head -n "$mru_count" > "$conf_tmp" && mv "$conf_tmp" "$conf_cache"
    echo "$result"
else
    return 1
fi
