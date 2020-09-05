### Vars
##
FZF_SIMPLE_PREVIEW='printf -- "%s " {}'
# fzf supports wrapping itself. # | command fold -s -w $FZF_PREVIEW_COLUMNS'
# << was bad for dash, no <<< in dash
##
### Functions
function fz-empty() {
    fnswap fzf-noempty fzf fz "$@"
}
function fz() {
    # "Use `fnswap fzf-noempty gq fz` to get the final command for use in other envs
    local opts
    opts=(${(@)fz_opts}) #Don't quote it or it'll insert empty args
    test -n "$fz_no_preview" || opts+=(--preview "$FZF_SIMPLE_PREVIEW" --preview-window down:7:wrap:hidden)
    
    # FNSWAP: fzf-noempty
    SHELL="${FZF_SHELL:-$(rp dash)}" fzf-noempty "${(@)opts}" "$@" # moved options to FZF_DEFAULT_OPTS
    # --select-1 
}
function fzf-noempty() {
    local in="$(</dev/stdin)"
    test -z "$in" && (exit 130) || { ec "$in" | SHELL="${FZF_SHELL:-$(rp dash)}" fzf --exit-0 "$@" }
}
