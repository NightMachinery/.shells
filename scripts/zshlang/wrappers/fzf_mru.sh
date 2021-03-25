#!/bin/dash
# @deps: GNU coreutils prefixed with 'g'

echo() {
    printf -- "%s\n" "$*"
}

mru_count="${fzf_mru_count:-90}"
mru_iteration_count="${fzf_mru_iteration_count:-5}"
mru_minquery="${fzf_mru_minquery}"
if test -n "$mru_minquery" ; then
    eval last=\${$#}
    if test "$(echo "${last}" | wc -c)" -lt "$mru_minquery" ; then
        mru_iteration_count=0
    fi
fi
if test -z "${fzf_mru_nostdin}" ; then
    input="$(cat)" || return $?
else
    input="$(eval "${FZF_DEFAULT_COMMAND}")" || return $?
fi

context="${fzf_mru_context:-${context}}"
if test -z "$context" ; then
    # @warn setting the context with this heuristic can introduce non-existent cached entries into different contexts
    context="$(echo "$input" | ghead -n 2 | md5sum | gawk '{ORS="" ; print $1}')" || return $?

    if test -z "$context" ; then
    echo "$0: You need to provide a context. (Automatic generation of a context failed.)" >&2
    exit 1
    fi
fi


conf_dir="$HOME/.cache/fzf_mru"
mkdir -p "$conf_dir" || (
    echo "$0: could not make the cache dir: '$conf_dir'" >&2
    return 1
)
conf_cache="$conf_dir/${context}"
touch "$conf_cache" || (
    echo "$0: could not touch the cache file: '$conf_cache'" >&2
    return 1
)
conf_tmp="${conf_cache}.tmp"

# Removing duplicate lines: seen is an associative-array that Awk will pass every line of the file to. If a line isn't in the array then seen[$0] will evaluate to false. (The name 'seen' does not matter.)
result="$(echo "$input" | cat "$conf_cache" - | gawk '!seen[$0]++' | fzf --tiebreak=index "$@")" || return $?
if test -n "$result" ; then
    echo "$result"
    ##
    echo "$result" | ghead -n "$mru_iteration_count" | cat - "$conf_cache" | gawk '!seen[$0]++' | ghead -n "$mru_count" > "$conf_tmp" || return $?
    ##
    mv "$conf_tmp" "$conf_cache"
else
    return 1
fi
