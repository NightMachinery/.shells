fmn() {
    man -k . | fzf --prompt='Man> ' | awk '{print $1}' | gxargs -r man
}
