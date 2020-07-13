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
    set-volume 0
    silence mpv "$@" &
    local mympv=$!
    sleep 5
    # local i=0
    while true
    do
        set-volume 0
        (( $(getidle-darwin) >= 0.2 )) &&  {
            cleanbuffer
            kill -9 -$mympv # '-' makes it kill the whole process group. It's necessary.
            # pk mpv
            # print -n -- "\rKilling mpv for the ${i}th time ..."
            # i=$((i+1))
            # sleep 10 #interferes with silencing the volume
            break
            }
        sleep 0.1
    done
}
function yta() {
    mdoc "$0 <str> ...
Searches Youtube and plays the result as audio." MAGIC

    mpv --ytdl-format=bestaudio ytdl://ytsearch:"$*"
}
function mpv-cache() {
    mpv --force-seekable=yes --cache=yes --cache-secs=99999999 "$@"
}
function mpv-stream() {
    local file="$@[-1]"
    local opts=( "${@[1,-2]}" )
    # cache has been disabled in mpv.conf
    mpv $opts[@] appending://"$file"
}
aliasfn mpvs mpv-stream
##
function retry-mpv() {
    # retry-eval "sleep 5 ; mpv --quiet $@ |& tr '\n' ' ' |ggrep -v 'Errors when loading file'"
    retry mpv-partial "${(Q)@}"
}
function mpv-partial() {
    ecerr "$0 started ..."
    local l=''
    # --quiet
    mpv "$@" |& {
        while read -d $'\n' -r l
        do
            color 50 200 50 "$l" >&2
            [[ "$l" =~ '(Cannot open file|Errors when loading file|Invalid NAL unit size)' ]] && { return 1 }
        done
    }
    return 0
}
##
