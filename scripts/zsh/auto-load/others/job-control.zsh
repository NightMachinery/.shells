inbg() {
    { eval "$(gquote "$@")" & }
    disown &>/dev/null  # Prevent whine if job has already completed
}
awaysh() inbg silent "$@"
function away() {
    nohup -- "$@" &
    disown &>/dev/null  # Prevent whine if job has already completed
}
