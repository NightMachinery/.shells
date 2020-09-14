function ffz() {
    # @todo2 We can bypass the interactive selection if the score of the top match is high enough compared to the second-best match, and have a `zi` that disables this auto-bypass.
    setopt localoptions pipefail
    local query="$*" sel

    # zoxide query --list "$query" | fz --no-sort -1 | inargsf cd
    local fzf_opts=()
    isI || fzf_opts=(--filter "$query")
    sel="$(zoxide query --list | fz --no-sort -1 --query "$query" "$fzf_opts[@]" | head -1)" || return 1
    cd "$sel"
}
aliasfn z ffz
