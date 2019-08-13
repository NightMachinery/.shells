function loop() {
    local inter=${lo_s:-1}
    local cmd="$(gquote "${@}")"
    # >&2 color 255 73 28 "Looping $cmd with interval $inter"
    >&2 chalk -t "{rgb(255,255,255).bgRgb(0,30,230) Looping {rgb(0,30,230).bgRgb(255,255,255) $cmd} with interval {rgb(255,73,28) $inter}}"
    test -z "$lo_noinit" || { color 0 255 100 "$(colorbg 255 255 255)Skipping first iteration" >&2 ; sleep $inter }
    while true
    do eval "$cmd"
       # color 255 40 10 "Ran $cmd
# Sleeping for $inter ..." >&2
       chalk-animation neon --duration $((inter*1000)) "Ran $cmd
Sleeping for $inter ..."
       # sleep $inter
    done
}
inbg() {
    { eval "$(gquote "$@")" & }
    disown &>/dev/null  # Prevent whine if job has already completed
}
awaysh() inbg silent "$@"
function away() {
    ruu 'nohup --' "$@" &
    disown &>/dev/null  # Prevent whine if job has already completed
}
