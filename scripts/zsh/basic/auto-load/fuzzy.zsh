## Vars
FZF_SIMPLE_PREVIEW='printf -- "%s " {} | command fold -s -w $FZF_PREVIEW_COLUMNS' # << was bad for dash, no <<< in dash
## Functions
function fz-empty() {
    fnswap fzf-noempty fzf fz "$@"
}
function fz() {
    local opts
    opts=(${(@)fz_opts}) #Don't quote it or it'll insert empty args
    test -n "$fz_no_preview" || opts+=(--preview "$FZF_SIMPLE_PREVIEW" --preview-window down:7:hidden)
    
    # FNSWAP: fzf-noempty
    SHELL="${FZF_SHELL:-$(rp dash)}" fzf-noempty --bind 'shift-up:toggle+up,shift-down:toggle+down,tab:toggle,shift-tab:toggle+beginning-of-line+kill-line,alt-/:toggle-preview,ctrl-j:toggle+beginning-of-line+kill-line,ctrl-t:top,ctrl-a:select-all' --color=light --multi --hscroll-off 99999 "${(@)opts}" "$@"
    # --select-1 
}
function fzf-noempty() {
    local in="$(</dev/stdin)"
    test -z "$in" && (exit 130) || { ec "$in" | SHELL="${FZF_SHELL:-$(rp dash)}" fzf --exit-0 "$@" }
}
