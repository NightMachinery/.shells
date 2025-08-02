## Vars
export mpv_ipc=~/tmp/.mpvipc # https://github.com/AN3223/dotfiles/blob/master/.config/mpv/scripts/multisocket.lua
typeset -g MPV_AUDIO_NORM=--af-add='dynaudnorm=g=5:f=250:r=0.9:p=0.5'     # audio volume normalizer. See https://github.com/mpv-player/mpv/issues/6563
# typeset -g MPV_AUDIO_NORM=--af-add='loudnorm=I=-16:TP=-3:LRA=4' # alt
export MPV_HOME="${MPV_HOME:-$HOME/.config/mpv}"

## Functions
function mpv-manga {
    bella_zsh_disable1

    local args=()
    h_mpv-args "$@" @RET
    set --

    pushf "$MPV_HOME" # Probably because of --no-config, mpv can't expand '~~/'. So we use $MPV_HOME directly. (Shader hotkeys need '~~/' to work.)
    {
        mpv_ipc=~/tmp/.mpv.manga mpv --no-config --load-scripts=no --script=$MPV_HOME/mpv-manga-reader/manga-reader.lua --image-display-duration=inf --input-conf=$MPV_HOME/input.conf --reset-on-next-file=video-pan-x,video-pan-y --video-align-x=1 --video-align-y=-1 --video-zoom=1 --fs --glsl-shaders="$MPV_HOME/shaders/Anime4K_3.0_Denoise_Bilateral_Mode.glsl:$MPV_HOME/shaders/Anime4K_3.0_Upscale_CNN_M_x2_Deblur.glsl" "$args[@]"
        # shader: 1,4 are good, I chose 4.
    } always { popf }
}

function command-mpv {
    bella_zsh_disable1

    # kitty injects its corrupt PATH into our shells
    # if test -e /Applications/mpv.app/Contents/MacOS/mpv ; then
        # command /Applications/mpv.app/Contents/MacOS/mpv "$@"
    # else
        command mpv "$@"
    # fi
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
            args+="$(grealpath --canonicalize-existing -- "$i")" @TRET
        else
            args+="$i"
        fi
    done

    tty-title "$first" || true
}


function mpv {
    bella_zsh_disable1

    local isStreaming="$mpv_streaming"

    local opts=()
    test -z "$isStreaming" && opts+="$MPV_AUDIO_NORM"

    if ! { isLocal && isMe } ; then
        opts+=(--script-opts-add=autotag-enabled=no)
    fi

    if isAppleSilicon ; then
        opts+=(--macos-force-dedicated-gpu=yes)
    fi

    local args=()
    assert h_mpv-args "$@" @RET
    set --

    revaldbg $proxyenv command-mpv $opts[@] --sub-auto=fuzzy --fs --input-ipc-server="$mpv_ipc" "${(@)args}"
}
aliasfn mpv-noconfig command mpv --no-config --load-scripts=no
###
function hear-prev {
    hear-do playlist-prev @RET

    hear-get
    #: @raceCondition This usually returns the file that was playing before this command.
}

function hear-next {
    hear-do playlist-next @RET

    hear-get
    #: @raceCondition This usually returns the file that was playing before this command.
}

aliasfn mpv-prev fnswap hear-do mpv-do hear-prev
aliasfn mpv-next fnswap hear-do mpv-do hear-next

function hear-seek-begin {
    hear-do seek 0 absolute-percent
}
##
function h-mpv-get-prop {
    local ipc_do_cmd="$1"
    local prop="${2:-path}"

    local res
    res="$($ipc_do_cmd get_property "$prop")" @TRET
    res="$(ec "${res}" | jq --raw-output -e .data)" || {
        ecerr "$0: Failed to parse JSON response for property '$prop':"$'\n'"$res"
        return 1
    }

    if [[ "$prop" == 'path' ]] ; then
        res="$(ntag-recoverpath "$res")" @STRUE
    fi

    ec "$res" |
        cat-copy-rtl-if-tty
}

function hear-get {
    h-mpv-get-prop hear-do "${@}"
}
alias 'hgg'=hear-get

function mpv-get {
    h-mpv-get-prop mpv-do "${@}"
}
##
function mpv-do {
    : "See https://mpv.io/manual/stable/#list-of-input-commands"

    local cmd
    cmd="$(arrJ "$@")" @RET

    local res
    res="$(revaldbg mpv-rpc "$cmd")" @TRET
    res="$(ec "${res}" | jq .)" || {
        ecerr "$0: Failed to parse JSON response:"$'\n'"$res"
        return 1
    }

    err="$(ec "${res}" | jq -re .error)" || {
        ecerr "$0: failed to get error:"$'\n'"$res"
        return 1
    }

    if [[ "$err" == "success" ]] ; then
        ec "${res}"

        return 0
    else
        ecerr "$0: error: ${err}"

        return 1
    fi
}

function mpv-rpc {
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
aliasfn hear-rpc mpv-rpc-audio mpv-rpc
aliasfn hear-do mpv-rpc-audio mpv-do

aliasfn hear-shuffle hear-do playlist-shuffle
aliasfn mpv-play-toggle mpv-do keypress space
aliasfn hear-play-toggle hear-do keypress space
aliasfn hear-play-on hear-do set pause no
aliasfn hear-play-off hear-do set pause yes
aliasfn mpv-play-on fnswap hear-do mpv-do hear-play-on
aliasfn mpv-play-off fnswap hear-do mpv-do hear-play-off

aliasfn hear-shuffle hear-do playlist-shuffle # or just press 'k'

function hear-loadfile {
    local url="$1"
    local mode="${2:-${mpv_load_mode:-replace}}"
    local opts=( "${@:3}" )
    #: * =replace=: Stop playback of the current file, and play the new file immediately.
    #: * =append-play=: Append the file, and if nothing is currently playing, start playback. (Always starts with the added file, even if the playlist was not empty before running this command.) This will not skip the currently playing file.
    #: * =append=: Append the file to the playlist.
    #:
    #: The append modes add all the other files of the dir to the playnext, too (possibly because of my scripts).
    local mpv_command="${mpv_load_command:-loadfile}"

    assert-args url
    if test -e "$url" ; then
        url="$(grealpath -- "$url")" @TRET
    fi

    if [[ "${mpv_command}" == 'loadfile' ]] ; then
        reval-ecgray hear-autoload-enable
    fi

    revaldbg hear-do "${mpv_command}" "${url}" "$mode" "${opts[@]}"
    revaldbg hear-play-on
}
aliasfn hear-open hear-loadfile
aliasfn hlo hear-loadfile
aliasfn mpv-loadfile fnswap hear-do mpv-do hear-loadfile
aliasfn mpv-open mpv-loadfile

function mpv-script-message-to {
    local script="$1" msg="$2"
    assert-args script msg @RET
    mpv-rpc "$(jq -nc --arg script "$script" --arg msg "$msg" '["script-message-to", $script, $msg]')"
}
aliasfn hear-script-message-to mpv-rpc-audio mpv-script-message-to

aliasfn mpv-autoload-enable mpv-script-message-to autoload enable
aliasfn mpv-autoload-disable mpv-script-message-to autoload disable
aliasfn hear-autoload-enable mpv-rpc-audio mpv-autoload-enable
aliasfn hear-autoload-disable mpv-rpc-audio mpv-autoload-disable

function hear-load-playlist {
    {
        reval-ecgray hear-autoload-disable
        sleep 0.2 #: to make =hear-autoload-disable= take effect
        mpv_load_command=loadlist reval-ecgray hear-loadfile "$@"
        sleep 0.2 && revaldbg hear-seek-begin
    } always {
        #: We can't re-enable =autoload=, or it will autoload files when the next file in the playlist is opened.
        # awaysh eval 'sleep 1 ;  hear-autoload-enable'
    }
}


function hear-loadfile-begin {
    hear-loadfile "$@" @RET
    if true ; then
        retry_sleep=0.05 retry-limited 100 hear-seek-begin
    else
        #: @deprecated
        sleep 0.3 && #: @raceCondition can't seek until the file has been loaded
            revaldbg hear-seek-begin
    fi
}
###
function play-and-trash(){
    #aliased to pat
    mpv "$@" && trs "$1"
}
aliasfn mpv-tag mpv --script-opts-add=autotag-enabled=yes
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

function mpv-cache {
    mpv --force-seekable=yes --cache=yes --cache-secs=99999999 "$@"
}

function mpv-stream {
    local file="$(grealpath -- "$@[-1]")"
    local opts=( "${@[1,-2]}" )
    # cache has been disabled in mpv.conf
    mpv_streaming=y mpv-notag $opts[@] appending://"$file"
}
aliasfn mpvs mpv-stream
##
function retry-mpv {
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

function mpv-imgseq {
    mpv "mf://*.png" --mf-fps=30
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

function mpv-bookmark-cleaned {
    local bookmark_file="${mpv_bookmarks}"

    cat "${bookmark_file}" |
        tac |
        perl -nle 'm/^\S+\s*(.+)$/ && !$seen{$1}++ && print $_' |
        tac
}

function mpv-bookmark-cleanup {
    local bookmark_file="${mpv_bookmarks}"

    mpv-bookmark-cleaned "${bookmark_file}" |
        sponge "${bookmark_file}"
}

function mpv-bookmark-fz {
    local engine=("${mpv_bookmark_engine:-mpv}")
    local bookmark_file="${mpv_bookmarks}"

    local bookmarks
    # bookmarks="$(cat "$bookmark_file")" @TRET
    bookmarks="$(mpv-bookmark-cleaned "$bookmark_file")" @TRET

    bookmarks="$(ec "$bookmarks" | fz --tac --no-sort)" @RET

    local timestamps
    timestamps=(${(@f)"$(ec "$bookmarks" | awkn 1)"}) @TRET
    local files
    files=(${(@f)"$(ec "$bookmarks" | perl -lne '@F = split(/ /, $_, -1) ; print "@F[1 .. $#F]"')"}) @TRET
    #: The perl oneliner drops the first field when split on whitespace/

    if (( ${#files} > 1 )); then
        ecerr "$0: multiple selections not supported"
    fi

    reval-ec "${engine}" --start="${timestamps[1]}" "${files[1]}"
}

function hear-bookmark-fz {
    mpv_bookmark_engine=hear-noipc mpv-bookmark-fz "$@"
}


function h-hear-bookmark-loader {
    # usage: h-hear-bookmark-loader --start=<time> <file>
    local start_arg="$1"
    local file="$2"

    local time="${start_arg#--start=}"
    #: `#`: The smallest matching pattern is deleted.
    #: [[id:88424c96-3482-4ed1-8178-6cb31240a041]]

    hear-loadfile "$file" "replace" "start=$time"
}

function hlo-bookmark-fz {
    mpv_bookmark_engine=h-hear-bookmark-loader mpv-bookmark-fz "$@"
}
##
function mpv-tui {
    : "@seeAlso command mpv --no-config ..."

    mpv-notag --profile=sw-fast --vo=tct "$@"
}
##
function mpv-cheatsheet {
    icat "${nightNotesPublic}/cheatsheets/mpv/mpbindings_big.png"
}
##
function mpv-progress {
    mpv -osd-msg1='${osd-ass-cc/0}{\\pos(10,-6)}{\\fs15}${osd-sym-cc} {\\fs15}${time-pos} / ${playtime-remaining}    ${percent-pos}%' "$@"
}
alias m='mpv-progress'
##
