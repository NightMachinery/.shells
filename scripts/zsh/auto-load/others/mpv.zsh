## Vars
export mpv_ipc=~/tmp/.mpvipc

## Functions
function mpv() {
    command mpv --sub-auto=fuzzy --fs --input-ipc-server="$mpv_ipc" "${(0@)$(rpargs "$@")}"
}
mpv-get() {
    <<<'{ "command": ["get_property", "'"${1:-path}"'"] }' socat - "${2:-$mpv_audio_ipc}"|jq --raw-output -e .data
}
mpv-getv() mpv-get "$1" "$mpv_ipc"
function play-and-trash(){
    #aliased to pat
    mpv "$@" && trs "$1"
}
function mpv-noidle() {
    silence mpv "$@" &
    local mympv=$!
    sleep 5
    # local i=0
    while true
    do
        set-volume 0
        (( $(getidle-darwin) >= 0.7 )) &&  {
            cleanbuffer
            kill -9 -$mympv # '-' makes it kill the whole process group. It's necessary.
            # pk mpv
            # print -n -- "\rKilling mpv for the ${i}th time ..."
            # i=$((i+1))
            # sleep 10 #interferes with silencing the volume
            break
            }
    done
}
