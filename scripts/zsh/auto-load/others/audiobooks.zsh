dir2ab() {
    mdocu '<dir> [<output_name>]
Joins the audio files in <dir>, removes the originals, generates a podcast feed, and returns the url.' MAGIC
    local abdir="$1"
    local out="${2:-${abdir:t}}" base=~/Downloads/
    # re dvar abdir out argv
    pushf $base
    abdir="$(realpath --relative-to $base "$abdir")"
    local tmp=$abdir/tmp_dir2ab/
    mv $abdir/**/*.(${(j.|.)~audio_formats})(D) $tmp
    audio-join $abdir/$out $tmp/*(D)
    ecdbg Audio joined with $?
    local covers=() metadata desc=''
    metadata="$(serr fetch-ebook-metadata --title "$out" --timeout 120 --cover $abdir/night6cover.png)"
    unset match
    [[ "$metadata" =~ 'Comments\s+:(.*)' ]]
    desc="$(<<<"$match[1]" html-to-text)"
    [[ -e "$abdir/night6cover.png" ]] && covers+="$abdir/night6cover.png" ||         covers=($abdir/*.(jpg|png)(N))
    (( ${#covers} )) && {
        covers=( --image $covers[1] )
    }
    genRSS.py -d $abdir -e "${aj_out:e}" -t "$out" -p "${desc:-The night is fair ...}" -o $abdir/feed.rss $covers[@] --host 'http://lilf.ir:8080'
    trs $tmp
    get-dl-link $abdir/feed.rss
}
