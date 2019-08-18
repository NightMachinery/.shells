# imports filesystem.zsh
###
ABBase=~/Downloads/
###
dir2ab() {
    mdocu '<dir> [<output_name>]
Joins the audio files in <dir>, removes the originals, generates a podcast feed, and returns the url.' MAGIC
    local abdir="$1"
    local out="${2:-${abdir:t}}" base="$ABBase"
    #re dvar base abdir out
    #dact arger "$@"
    abdir="$(realpath --relative-to $base "$abdir")"
    pushf $base
    [[ "$(realpath "$abdir")" == "$(realpath "$base")" ]] && {
	>&2 color red abdir is the base. You probably do not want this. Aborting 33.
		return 33
    }
    local tmp=$abdir/tmp_dir2ab/
    rename-numbered $tmp $abdir/**/*.(${(j.|.)~audio_formats})(D)
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

movie2ab() {
    mdocu '<link> [<title>=(will be randomly generated)]
Downloads a file, converts it to m4a and creates a podcast out of it.' MAGIC
    local base="$ABBase" link="$1" title="${2:-$(xkcdpass)}"
    local dir=$base/julia/movie2ab/"$title"/
    pushf $dir
    aa "$1"
    local dled=(**/*.(${(j.|.)~media_formats}))
    ffmpeg -i $dled[1] out.m4a
    trs *~out.m4a
    dir2ab .
    popf
}
