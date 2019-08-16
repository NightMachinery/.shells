swap-audio() {
    ffmpeg -i "$1" -i "$2" -c:v copy -map 0:v:0 -map 1:a:0 -shortest "$3"
}
audio-join() { ffmpeg -i "concat:${(j:|:)@[2,-1]}" -acodec copy $1 }
