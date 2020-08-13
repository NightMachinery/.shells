function 265to264() {
    ffmpeg -i "$1" -map 0 -c:s copy -c:v libx264 -crf "${2:-18}" -c:a copy -preset "${3:-medium}" "${1:r}_x264.mkv"
    #-map_metadata 0
}
function mp3-to-mp4() (
    B=$(basename "$1"); D=$(dirname "$1");
    ffmpeg -loop 1 -i "$2" -i "$1" -pix_fmt yuv420p -c:v libx264 -crf 16  -c:a libfdk_aac -vbr 5 -preset veryslow -vf pad="width=ceil(iw/2)*2:height=ceil(ih/2)*2:x=0:y=0:color=black" -shortest "${3:-$D/${B%.*}}.mp4"
    # -c:a copy -r 1
)
function rloop_vid() (
    ffmpeg -i "$1" -filter_complex "[0:v]reverse,fifo[r];[0:v][r] concat=n=2:v=1 [v]" -map "[v]" "$1_rloop.${2:-mp4}"
)
function imgseq2vid() {
    local framerate="${imgseq2vid_r:-30}"
    local crf="${imgseq2vid_crf:-25}"
    local out="${imgseq2vid_out:-${imgseq2vid_o:-out.mp4}}"
    local input="${1}"
    test -z "$input" && input='%06d.png' # 000001.png ... # we can also use globs with `-pattern_type glob` but this seems safer (glob might lose ordering?). I'm tired :D

    local first="$(command ls|head -1)"
    convert -size "$(convert $first -ping -format "%wx%h" info:)" 'xc:rgba(255,255,255, 1)' white.png

    ffmpeg -threads 0 -loop 1 -i white.png -thread_queue_size 10000 -framerate $framerate -i $input -filter_complex "overlay=shortest=1,pad=width=ceil(iw/2)*2:height=ceil(ih/2)*2:x=0:y=0:color=white" -c:v libx265 -crf $crf -movflags +faststart -r $framerate $out
    # don't use `-f hevc`, it'll break the timestamps. Prob a bug upstream.
}
