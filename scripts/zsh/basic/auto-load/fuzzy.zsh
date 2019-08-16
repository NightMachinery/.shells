## Vars
FZF_SIMPLE_PREVIEW='<<<{} command fold -s -w $FZF_PREVIEW_COLUMNS'
## Functions
fz() {
    local opts
    opts=(${(@)fz_opts}) #Don't quote it or it'll insert empty args
    test -n "$fz_no_preview" || opts+=(--preview "$FZF_SIMPLE_PREVIEW" --preview-window down:7:hidden)
    dvar opts
    fzf-noempty --bind 'tab:toggle,shift-tab:toggle+beginning-of-line+kill-line,?:toggle-preview,ctrl-j:toggle+beginning-of-line+kill-line,ctrl-t:top' --color=light --select-1 --exit-0 --multi --hscroll-off 99999 "${(@)opts}" "$@"
}
fzf-noempty() {
    local in="$(</dev/stdin)"
    test -z "$in" && (exit 130) || { ec "$in" | fzf "$@" }
}
