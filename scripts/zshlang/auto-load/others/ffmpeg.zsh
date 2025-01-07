##
function vid-fix {
    local i="${1:?}" o="$2"

    if test -z "$o" ; then
        # o="${i:r}_fixed.${i:e}"
        o="${i:r}_fixed.${vid_fix_ext:-mp4}"
    fi
    reval-ec ffmpeg -err_detect ignore_err -i "$i" -c copy -movflags faststart "$o"
}
##
typeset -g h_vid_crop_vclass="crop=in_w-325:in_h-100:0:100"

function vid-crop-vclass {
    local i="${1}" o="$2"
    assert-args i @RET

    if test -z "$o"
    then
        o="${i%.*}_cropped.${vid_crop_ext:-mp4}"
    fi

    reval-ec ffmpeg -i "$i" -vf "${h_vid_crop_vclass}" -c:a copy -movflags faststart "$o"
}

function vid-crop-vclass-preview {
    local i="${1:?}"
    # Preview the cropped video using ffplay
    ffplay -i "$i" -vf "$h_vid_crop_vclass"
}
##
function merge-mp3() {
    local out="${merge_mp3_out:-${merge_mp3_o:-merged.mp3}}"
    { test -z "$out" } && { # redundant
        ecerr "$0: Empty output name. Aborting"
        return 1
    }
    test -z "${out:e}" && out="${out}.mp3"
    local inputs=() i input_len="${#@}"
    for i in "$@" ; do
        inputs+=( -i "$i" )
    done

    reval-ec ffmpeg "$inputs[@]" -filter_complex amix=inputs="${input_len}":duration=longest "$out"
}

swap-audio() {
    ffmpeg -i "$1" -i "$2" -c:v copy -map 0:v:0 -map 1:a:0 -shortest "$3"
}

function audio-join {
    : "@alt audio-join2"
    mdocu '<output> <audio-file> ...
Joins the <audio-file>s into <output>. Automatically sets the extension of <output>.
Outs: aj_out -> The output file. (Might be relative.)' MAGIC

    aj_out="${1:r}.${2:e}" # GLOBAL
    ffmpeg -i "concat:${(j:|:)@[2,-1]}" -acodec copy "$aj_out"
}

audio-join2() {
    : '<output> <audio-file> ...'
    (( $#@ < 2 )) && { ecerr "$0: Not enough arguments" ; return 1 }
    aj_out="${1:r}.${2:e}" # GLOBAL
    shift

    local concat="$(gmktemp)"
    {  for f in "$@"; do echo "file ${(qq)$(grealpath -- $f)}"; done } > $concat # https://www.ffmpeg.org/ffmpeg-utils.html#Quoting-and-escaping
    revaldbg ffmpeg -f concat -safe 0 -i "$concat"  -c copy "$aj_out"
    dact cat $concat
    command rm "$concat"
}
##
function 2mp3 {
    jglob
    local i="$1"

    ffmpeg -i $i ${i:r}.mp3
}
##
function ffmpeg-to264 {
    ffmpeg_f=x264 ffmpeg-convert "$@"
}

function ffmpeg-to265 {
    ffmpeg_f=x265 ffmpeg-convert "$@"
}

function ffmpeg-convert {
    #: @alt [agfi:hb265]
    ##
    local input="$1" crf="${2:-18}" preset="${3:-medium}"
    local encoder_family="${ffmpeg_f:-x265}"
    local dest="${ffmpeg_o:-${1:r}_${encoder_family}.mp4}"
    local hw_decode_p="${ffmpeg_hw_decode_p:-y}"
    #: =-hwaccel auto= enables hardware acceleration for input decoding. It might not actually help performance in my testing on Air M2.

    local hw_encode_p="${ffmpeg_hw_encode_p:-n}"
    #: > It is hard to get an objective apples to apples comparison when it comes to quality of Handbrake's SW and HW encoding settings, but when I tried to get a similar quality encode Handbrake SW and HW encoding, the file size of the HW encode was a lot larger than the SW encode.

    ensure-array ffmpeg_opts
    local opts=("${ffmpeg_opts[@]}") encoder
    if [[ "$encoder_family" == 'x265' ]] ; then
        if bool "$ffmpeg_hw_encode_p" && isAppleSilicon ; then
            encoder='hevc_videotoolbox'
            opts+=(-b:v 412k)
            #: VideoToolbox does not support constant quality (CRF) encoding. Bit rate, =-b:v ...=, is your main lever to balance size vs quality
        else
            encoder='libx265'
        fi
    elif [[ "$encoder_family" == 'x264' ]] ; then
         if bool "$ffmpeg_hw_encode_p" && isAppleSilicon ; then
            encoder='h264_videotoolbox'
        else
            encoder='libx264'
        fi
    else
        @NA
    fi

    local opts_input=()
    if bool "$hw_decode_p" ; then
        opts_input+=(-hwaccel auto)
    fi

    reval-ec ffmpeg "${opts_input[@]}" -i "$input" -map 0 -c:s copy -c:v "$encoder" -crf "$crf" -c:a copy -movflags faststart -preset "$preset" "${opts[@]}" "$dest"
    # -map_metadata 0
}
##
function mp3-to-mp4 {
    B=$(basename "$1"); D=$(dirname "$1");
    ffmpeg -loop 1 -i "$2" -i "$1" -pix_fmt yuv420p -c:v libx264 -crf 16  -c:a libfdk_aac -vbr 5 -preset veryslow -vf pad="width=ceil(iw/2)*2:height=ceil(ih/2)*2:x=0:y=0:color=black" -shortest "${3:-$D/${B%.*}}.mp4"
    # -c:a copy -r 1
}

function rloop_vid {
    ffmpeg -i "$1" -filter_complex "[0:v]reverse,fifo[r];[0:v][r] concat=n=2:v=1 [v]" -map "[v]" "$1_rloop.${2:-mp4}"
}
##
function ffmpeg-to-audio {
    ensure-array audio_codec_opts
    local codec_opts=("${audio_codec_opts[@]}")

    ensure-array ffmpeg_codec_opts
    local ffmpeg_opts=("${ffmpeg_codec_opts[@]}")

    local audio_extension="${audio_extension:-m4a}"
    local audio_codec="${audio_codec:-aac}"
    local print_out_p="${ffmpeg_print_out_p:-n}"

    local input="${1}"
    local output="${2:-${input:r}.${audio_extension}}"

    # Check if the input file exists
    if [[ ! -f "${input}" ]]; then
        ecerr "Input file not found: ${input}"
        return 1
    fi

    # Build ffmpeg options
    ffmpeg_opts+=("-c:a" "${audio_codec}")
    ffmpeg_opts+=("${codec_opts[@]}")
    ffmpeg_opts+=("-movflags" "+faststart")

    # Perform the conversion
    assert ffmpeg -hide_banner -loglevel error -i "${input}" "${ffmpeg_opts[@]}" -y "${output}" >&2 @RET

    # Print the output path if print_out_p is set
    if bool "${print_out_p}"; then
        ec "${output}"
    fi
}

function to-m4a {
    local input="${1}"
    local output="${2:-${input:r}.m4a}"

    # Set audio codec and extension
    local audio_extension="m4a"
    local audio_codec
    if [[ "$(uname)" == "Darwin" ]]; then
        # Use Apple AudioToolbox AAC encoder for hardware acceleration
        audio_codec="aac_at"
    else
        # Use default AAC encoder
        audio_codec="aac"
    fi

    # Call the generalized function
    ffmpeg-to-audio "${input}" "${output}"
}

function to-mp3 {
    local input="${1}"
    local output="${2:-${input:r}.mp3}"

    # Set audio codec and extension
    local audio_extension="mp3"
    local audio_codec="libmp3lame"

    # Call the generalized function
    ffmpeg-to-audio "${input}" "${output}"
}
##
function imgseq2vid {
    local framerate="${imgseq2vid_r:-30}"
    local crf="${imgseq2vid_crf:-25}"
    local out="${imgseq2vid_out:-${imgseq2vid_o:-out.mp4}}"
    local input="${1}"
    test -z "$input" && input='%06d.png' # 000001.png ... # we can also use globs with `-pattern_type glob` but this seems safer (glob might lose ordering?). I'm tired :D

    local first="$(command ls|head -1)"
    convert -size "$(convert $first -ping -format "%wx%h" info:)" 'xc:rgba(255,255,255, 1)' white.png

    printz-quoted ffmpeg -threads 0 -loop 1 -i white.png -thread_queue_size 10000 -framerate $framerate -i $input -filter_complex "overlay=shortest=1,pad=width=ceil(iw/2)*2:height=ceil(ih/2)*2:x=0:y=0:color=white" -c:v libx265 -crf $crf -movflags +faststart -r $framerate $out
    # don't use `-f hevc`, it'll break the timestamps. Prob a bug upstream.
}
aliasfn imgseq2vidNI fnswap printz-quoted reval imgseq2vid
@opts-setprefix imgseq2vidNI imgseq2vid

function imgdirs2vid() {
    local framerate="${imgseq2vid_r:-30}"
    local crf="${imgseq2vid_crf:-25}"
    local input="${imgseq2vid_input}"
    test -z "$input" && input='%06d.png'
    
    local dirs=( "$@" ) dir
    for dir in $dirs[@] ; do
        if ! test -d "$dir" ; then
            ecerr "$0: Directory '$dir' does not exist. Skipping."
            continue
        fi
        dir="$(grealpath -- "$dir")"
        pushf $dir

        local out="${dir}.mp4"
        # @opts out "$out" @ imgseq2vidNI
        ffmpeg -threads 0 -thread_queue_size 10000 -framerate $framerate -i $input -c:v libx265 -pix_fmt yuv420p -crf $crf -movflags +faststart -r $framerate $out

        
        popf
    done
}
##
function vid2gif() {
    # https://superuser.com/questions/556029/how-do-i-convert-a-video-to-gif-using-ffmpeg-with-reasonable-quality
    #
    # https://engineering.giphy.com/how-to-make-gifs-with-ffmpeg/
    ##
    local i="$1"
    local o="${2:-${i:r}.gif}"
    assert-args i @RET

    ffmpeg -i $i -vf "fps=10,scale=320:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 "$o"
}
##
function ffmpeg-speed {
    local input="${1}" speedup_factor="${2:-2}"
    local output="${3:-${input:r}_s${speedup_factor}.${input:e}}"
    assert-args input speedup_factor output @RET

    if (( speedup_factor > 2 )) ; then
        ecerr "$0: The atempo filter is limited to using values between 0.5 and 2.0 (so it can slow it down to no less than half the original speed, and speed up to no more than double the input). If you need to, you can get around this limitation by stringing multiple atempo filters together manually. Continuing anyway ..."
    fi

    local speedup_factor_inv=$((1/speedup_factor))

    reval-ec ffmpeg -i "$input" \
        -filter_complex "[0:v]setpts=${speedup_factor_inv}*PTS[v];[0:a]atempo=${speedup_factor}[a]" \
        -map "[v]" -map "[a]" "$output"
}
##
function ffmpeg-record {
    #: [[file:~/.hammerspoon/init.lua::function whisper.getRecordCommand(outputFile)]]
    ##
    local output="${1}"
    if test -z "${output}" ; then
        output="$(gmktemp --suffix=.wav)"
    fi

    local timeout="${record_timeout:-300}"

    #: Press Ctrl-C to stop recording.
    #: Does NOT work well. I need to press Ctrl-C two times. I don't know why it works better when we invoke it from Hammerspoon.
    ffmpeg -f avfoundation -i ":0" -t "${timeout}" -ar 16000 -y "$output" >&2 || true

    ec "${output}" |
        cat-copy-if-tty
}

function h-trap-int-do-nothing {
    ecgray "$0: called"

    return 0
}

function sox-record {
    #: [[id:6f8e0395-154e-45e7-b34b-a74b2a4ecab7][signal/catch]]
    ##
    local output="${1}"
    if test -z "${output}" ; then
        output="$(gmktemp --suffix=.wav)"
    fi

    local timeout="${record_timeout:-300}"

    #: Press Ctrl-C to stop recording.

    #: Ignore SIGINT in the parent shell
    setopt localtraps
    trap '' INT

    local cmd
    cmd="$(gquote rec "${output}" trim 0 "${timeout}")" @TRET

    (
        #: @redundant Restore default SIGINT handling in the subshell
        trap - INT

        rec "${output}" trim 0 "${timeout}"
    ) </dev/null >&2 || true # ecgray "$0: sox rec exited: $?"

    # zsh -mfc "${cmd}" </dev/null >&2 || true

    #: It exits with 130 when we Ctrl-C it.

    #: Restore default SIGINT handling in the parent shell
    trap - INT

    ec "${output}" |
        cat-copy-if-tty
}
