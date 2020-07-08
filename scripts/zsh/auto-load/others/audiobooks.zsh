# imports filesystem.zsh
###
ABBase=~/Downloads/
###
function yab() {
	pushf "$ABBase/audiotalks"
	yaudio "$@"
	dir2ab .
	popf
}
noglobfn yab
##
aliasfn dir2ab da_nm=y dir2ab-merge
function dir2ab-merge() {
	mdoc "Usage: [da_nm=<not empty <-> no merge>] $0 <dir> [<output_name>]
	Joins the audio files in <dir>, removes the originals, generates a podcast feed, and returns the url." MAGIC
	local abdir="$(bottomdir $1)"
	local base="$ABBase"
	#re dvar base abdir out
	#dact arger "$@"
	abdir="$(realpath --relative-to $base "$abdir")"
	pushf $base
	local out="${2:-${abdir:t}}"
	[[ $out == . ]] && out=dot
	[[ "$(realpath "$abdir")" == "$(realpath "$base")" ]] && {
		>&2 color red abdir is the base. You probably do not want this. Aborting 33.
		return 33
	}
	local tmp=$abdir/tmp_dir2ab/
	local afiles=($abdir/**/*.(${(j.|.)~media_formats})(D))
	{ [[ -n "$da_nm" ]] || (( $#afiles == 1 )) } && {
		# we no longer use aj_out, no?
		local aj_out=${afiles[1]} #This might cause problems (i.e., genRSS might not find it)  if the orig file is in directories
		# local aj_out=$abdir/$out.${afiles[1]:e}
		# mv $afiles[1] $aj_out
	} || {
		rename-numbered $tmp $afiles[@]
		audio-join $abdir/$out $tmp/*(D)
		ecdbg Audio joined with $?
	}
	local covers=() metadata desc=''
	metadata="$(eval-memoi serr fetch-ebook-metadata --title "$out" --timeout 120 --cover $abdir/night6cover.png)"
	unset match
	[[ "$metadata" =~ 'Comments\s+:(.*)' ]]
	desc="$(<<<"$match[1]" html-to-text)"
	[[ -e "$abdir/night6cover.png" ]] && covers+="$abdir/night6cover.png" ||         covers=($abdir/*.(jpg|png)(N))
	(( ${#covers} )) && {
		covers=( --image $covers[1] )
	}
	# -e "${aj_out:e}"
	genRSS.py -d $abdir -e "${(j.,.)~media_formats}" -t "$out" -p "${desc:-The night is fair ...}" -o $abdir/feed.rss $covers[@] --host "${dl_base_url:-http://lilf.ir:8080}"
	trs $tmp
	get-dl-link $abdir/feed.rss
	popf
}

movie2ab() {
	mdocu '<link> ... [-t,--title=<title>]
	Downloads the file, converts it to audio and creates a podcast out of it.
	Does nor currently support multiple files.' MAGIC
	aa_2audio=y aa2ab "$@"
}
aa2ab() {
	mdocu 'aa_2audio=<convert-to-audio?> <link> ... [-t,--title=<title>]
	Downloads files, and creates a podcast out of them.' MAGIC
	typeset -A opts
	opts[-t]="$(xkcdpass)"
	zparseopts -A opts -K -E -D -M -title:=t t:
	local base="$ABBase" links=("${@}") title="${opts[-t]}"
	#re dvar links title
	local dir=$base/julia/movie2ab/"$title"/
	pushf $dir
	aa -Z "$links[@]"
	test -n "$aa_2audio" && { 
		local dled=(**/*.(${(j.|.)~video_formats}))
		local out=out.mp3
		ffmpeg -i $dled[1] $out
		trs *~$~out
	}
	dir2ab-merge .
	popf
}
