#!/bin/dash
# @deps: GNU coreutils prefixed with 'g' (as installed by Homebrew), GNU moreutils, fzf-tmux

echo() {
    printf -- "%s\n" "$*"
}
isKitty () {
    test -n "$KITTY_WINDOW_ID"
}
fzf() {
    if isKitty || test -n "$FORCE_NONINTERACTIVE" ; then
        command fzf "$@"
    else
        fzf-tmux -p90% "$@"
    fi
}

h_fzf_mru_deduplicate() {
    # fzf returns plain selections for --ansi input. Compare without SGR
    # colors so a cached plain row and its current colored row are one item;
    # keep the first row's original display bytes and the MRU ordering.
    command perl -MTerm::ANSIColor=colorstrip -ne 'print unless $seen{colorstrip($_)}++'
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
    context="$(echo "$input" | ghead -n 2)" || return $?

    if test -z "$context" ; then
    echo "$0: You need to provide a context. (Automatic generation of a context failed.)" >&2
    exit 1
    fi
fi
# md5sum is from coreutils
context="$(echo "$context" | md5sum | gawk '{ORS="" ; print $1}')" # protects against bad names, also good for privacy, but bad for readability

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

# Cached entries lead, with duplicates compared by their plain text.
result="$(echo "$input" | cat "$conf_cache" - | h_fzf_mru_deduplicate | fzf --tiebreak=index "$@" | sponge)" || return $?

if test -n "$result" ; then
    echo "$result"
    ##
    echo "$result" | ghead -n "$mru_iteration_count" | cat - "$conf_cache" | h_fzf_mru_deduplicate | ghead -n "$mru_count" > "$conf_tmp" || return $?
    ##
    mv "$conf_tmp" "$conf_cache"
else
    return 1
fi
