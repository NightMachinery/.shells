## Vars
export mpv_ipc=~/tmp/.mpvipc # https://github.com/AN3223/dotfiles/blob/master/.config/mpv/scripts/multisocket.lua
typeset -g MPV_AUDIO_NORM=--af-add='dynaudnorm=g=5:f=250:r=0.9:p=0.5'     # audio volume normalizer. See https://github.com/mpv-player/mpv/issues/6563
# typeset -g MPV_AUDIO_NORM=--af-add='loudnorm=I=-16:TP=-3:LRA=4' # alt
export MPV_HOME="${MPV_HOME:-$HOME/.config/mpv}"

## Functions
function mpv-manga() {
    local args=()
    h_mpv-args "$@" @RET
    set --

    pushf "$MPV_HOME" # Probably because of --no-config, mpv can't expand '~~/'. So we use $MPV_HOME directly. (Shader hotkeys need '~~/' to work.)
    {
        mpv_ipc=~/tmp/.mpv.manga mpv --no-config --load-scripts=no --script=$MPV_HOME/mpv-manga-reader/manga-reader.lua --image-display-duration=inf --input-conf=$MPV_HOME/input.conf --reset-on-next-file=video-pan-x,video-pan-y --video-align-x=1 --video-align-y=-1 --video-zoom=1 --fs --glsl-shaders="$MPV_HOME/shaders/Anime4K_3.0_Denoise_Bilateral_Mode.glsl:$MPV_HOME/shaders/Anime4K_3.0_Upscale_CNN_M_x2_Deblur.glsl" "$args[@]"
        # shader: 1,4 are good, I chose 4.
    } always { popf }
}

function command-mpv() {
    # kitty injects its corrupt PATH into our shells
    if test -e /Applications/mpv.app/Contents/MacOS/mpv ; then
        command /Applications/mpv.app/Contents/MacOS/mpv "$@"
    else
        command mpv "$@"
    fi
}

function h_mpv-args {
    : "initialize args=() before calling me"
    # Usage: h_mpv-args <arg> ...
    # Global OUT: args
    ##
    if (( $#args > 0 )) ; then
        ecerr "$0: args is not empty"
        return 1
    fi

    local first
    local i
    for i in "$@"
    do
        if test -e "$i" ; then
            if test -z "$first" ; then
                first="${i:t}"
            fi
            args+="$(grealpath --canonicalize-existing -- "$i")"
        else
            args+="$i"
        fi
    done

    tty-title "$first"
}


function mpv() {
    local isStreaming="$mpv_streaming"

    local opts=()
    test -z "$isStreaming" && opts+="$MPV_AUDIO_NORM"

    if ! { isLocal && isMe } ; then
        opts+=(--script-opts-add=autotag-enabled=no)
    fi

    local args=()
    h_mpv-args "$@" @RET
    set --

    revaldbg $proxyenv command-mpv $opts[@] --sub-auto=fuzzy --fs --input-ipc-server="$mpv_ipc" "${(@)args}"
}
aliasfn mpv-noconfig command mpv --no-config --load-scripts=no
###
function hear-prev {
    hear-do playlist-prev @RET

    hear-get
}

function hear-next {
    hear-do playlist-next @RET

    hear-get
}

aliasfn mpv-prev fnswap hear-do mpv-do hear-prev
aliasfn mpv-next fnswap hear-do mpv-do hear-next
##
function hear-get {
    hear-do get_property "${1:-path}" | jq --raw-output -e .data
}

function mpv-get {
    mpv-do get_property "${1:-path}" | jq --raw-output -e .data
}
##
function mpv-do {
    : "See https://mpv.io/manual/stable/#list-of-input-commands"

    local cmd
    cmd="$(arrJ "$@")" @RET

    mpv-rpc "$cmd"
}
function mpv-rpc () {
    local soc="${mpv_rpc_socket:-$mpv_ipc}" cmd="$1"
    assert-args soc cmd @RET
    assert test -e "$soc" @RET

    local cmd='{ "command": '$cmd' }'
    # the compacting is necessary :FACEPALMS:
    cmd="$(ec $cmd | jq --compact-output .)" || {
        ecerr "$0: bad json produced (bug?)"
        return 1
    }
    ecdbg "$0: cmd: $cmd"
    ec $cmd | socat - "$soc" | jq .
}
alias mpv-rpc-audio='mpv_rpc_socket="$mpv_audio_ipc" '
aliasfn hear-do mpv-rpc-audio mpv-do

aliasfn mpv-play-toggle mpv-do keypress space

aliasfn hear-play-toggle hear-do keypress space
aliasfn hear-shuffle hear-do playlist-shuffle # or just press 'k'
###
function play-and-trash(){
    #aliased to pat
    mpv "$@" && trs "$1"
}
aliasfn mpv-notag mpv --script-opts-add=autotag-enabled=no
aliasfn mpvn awaysh mpv-notag
function mpv-noidle() {
    set-volume 0
    silence mpv-notag "$@" &
    local mympv=$!
    sleep 5
    # local i=0
    while true
    do
        set-volume 0
        (( $(idle-get) >= 0.2 )) &&  {
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
    local file="$(realpath "$@[-1]")"
    local opts=( "${@[1,-2]}" )
    # cache has been disabled in mpv.conf
    mpv_streaming=y mpv-notag $opts[@] appending://"$file"
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
    mpv-stream "$@" |& {
        while read -d $'\n' -r l
        do
            color 50 200 50 "$l" >&2
            [[ "$l" =~ '(Cannot open file|Errors when loading file|Invalid NAL unit size)' ]] && { return 1 }
        done
    }
    return 0
}
##
function playtmp() {
    mkdir -p ~/tmp/delme/
    cp "$1" ~/tmp/delme/
    color 0 200 0 Copied "$1" to tmp
    fsay Copied to tmp
    pat ~/tmp/delme/"$1:t"
}

function mpv-imgseq() {
    mpv "mf://*.png" --mf-fps 30
}
aliasfn mpvi mpv-imgseq
##
function h_mpv-bookmark {
    local file="${1}" time="${2}"
    assert-args file time @RET

    local bookmark_file="${mpv_bookmarks}"
    # bookmark_file="${file:r}.bkm"

    sync-append "$bookmark_file" "$time $file"$'\n' @TRET

    bell-p2-target-acquired

    ec "added bookmark for time $time and file $(gquote-dq $file)"
}

function mpv-bookmark-fz {
    local bookmark_file="${mpv_bookmarks}"

    local bookmarks
    bookmarks="$(cat "$bookmark_file")" @TRET

    bookmarks="$(ec "$bookmarks" | fz --tac --no-sort)" @RET

    local timestamps
    timestamps=(${(@f)"$(ec "$bookmarks" | awkn 1)"}) @TRET
    local files
    files=(${(@f)"$(ec "$bookmarks" | perl -anE 'say "@F[1 .. $#F]"')"}) @TRET

    if (( ${#files} > 1 )); then
        ecerr "$0: multiple selections not supported"
    fi

    reval-ec hear --start="${timestamps[1]}" "${files[1]}"
}
##
