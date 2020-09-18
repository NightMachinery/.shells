function ffz() {
    ##
    # @retiredtodo2 We can bypass the interactive selection if the score of the top match is high enough compared to the second-best match, and have a `zi` that disables this auto-bypass.
    # DONE: We can also just cache the result for each query!
    ##
    setopt localoptions pipefail
    local query="$*" sel

    ##
    # local fzf_opts=()
    # isI || fzf_opts=(--filter "$query")
    # sel="$(zoxide query --list | fz --no-sort -1 --query "$query" "$fzf_opts[@]" | head -1)" || return 1
    ##
    # memoi-eval doesn't read from pipe
    sel="$( { serr zoxide query --list || true } | memoi_skiperr=y memoi_od=0 memoi_expire=0 memoi-eval fzp "$query" | head -1)" || return 1
    cd "$sel"
}
aliasfn z ffz
aliasfn zi deus ffz
