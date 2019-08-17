ffmpeg() {
    isI && command ffmpeg "$@" || command ffmpeg -loglevel warning "$@"
}
swap-audio() {
    ffmpeg -i "$1" -i "$2" -c:v copy -map 0:v:0 -map 1:a:0 -shortest "$3"
}
audio-join() {
    mdocu '<output> <audio-file> ...
Joins the <audio-file>s into <output>. Automatically sets the extension of <output>.
Outs: aj_out -> The output file. (Might be relative.)' MAGIC
    aj_out="$1.${2:e}"
    ffmpeg -i "concat:${(j:|:)@[2,-1]}" -acodec copy "$aj_out"
}
