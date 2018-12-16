function bii() {
    brew bundle --file=/dev/stdin <<<"brew \"$1\" ${@:2}"
}
function whh() {
    wh $(strip "`wh "$1"`" "$1: aliased to ")
}
function strip() {
    local STRING="${1#"$2"}"
    echo "${STRING%"$2"}"
}
function cee() {
    cat `which "$1"`
}
function setv() {
    #PORTME
    osascript -e "set volume output volume $1"
}
function 265to264() {
    ffmpeg -i "$1" -map 0 -c:s copy -c:v libx264 -crf "${2:-18}" -c:a copy -preset "${3:-medium}" "${1:r}_x264.mkv"
    #-map_metadata 0
}
function retry-eval() {
    retry eval "$@"
	  # until eval "$@" ; do
# 		    echo Retrying \'"$*"\' "..." 1>&2
# 		    sleep 1
# 	  done
}
function retry() {
	until "$@" ; do
		echo Retrying \'"$*"\' "..." 1>&2
		sleep 1
	done
}
function 2mobi() {
	ebook-convert "$1" "${1:r}.mobi"
}
function 22k() {
	2mobi "$1"
	2kindle "${1:r}.mobi"
}
function 2kindle() {
	mutt -s convert -a "$1" -- "${2:-fifya@kindle.com}" <<<hi
}
function ins() {
	eval-dl "bi $1" "ai $1"
}
function e() {
	echo "${pipestatus[@]}" "${PIPESTATUS[@]}"
}
function ncp() {
	cat | gnc -c localhost 2000
}
function magnet2torrent() {
    aria2c -d "${2:-.}" --bt-metadata-only=true --bt-save-metadata=true "$1"
}
it2prof() { echo -e "\033]50;SetProfile=$1\a" ; } # Change iterm2 profile. Usage it2prof ProfileName (case sensitive)
file-to-clipboard() {
    osascript \
        -e 'on run args' \
        -e 'set the clipboard to POSIX file (first item of args)' \
        -e end \
        "$@"
}
function rep() {
    "${@:2}" |& ggrep -iP "$1"
}
function y-stream() {
    y -f best  -o "%(title)s.%(ext)s" "$@" &
    local out=$(yic -f best --get-filename -o "%(title)s.%(ext)s" "$@")
    #We need to use yic or archived videos return nothing causing mpv to play * :D
    retry-eval "mpv --quiet '$out'* |& tr '\n' ' ' |ggrep -v 'Errors when loading file'"
    #mpv bug here
    # kill $!
    # kill $! is your friend :))
}
function dl-stream() {
    aria2c "$1" &
    #TODO
    #  "${2:i-iina}" ""
}
function set-fk-icon-size() {
    /usr/libexec/PlistBuddy -c "set FK_DefaultIconViewSettings:iconSize ${1:-128}" ~/Library/Preferences/com.apple.finder.plist # This is for Finderkit, i.e., dialogs.
                            }
function set-finder-icon-size() {
    /usr/libexec/PlistBuddy -c "set StandardViewSettings:IconViewSettings:iconSize ${1:-128}" ~/Library/Preferences/com.apple.finder.plist # This is for Finder itself.
}
function loop() { while true; do eval "${@:2}"; sleep ${1:-1}; done ; }
function rename-ebook() {
    local filename=$(basename "$1")
    local extension="${filename##*.}"
    local filename="${filename%.*}"
    local directory=$(dirname "$1")
    local meta="$(ebook-meta $1)"
    local newfilename="$(<<< $meta grep -i title|read -r a a b; echo $b) - $(<<< $meta grep -i author|read -r a a b; echo $b)"
    #local newfilename="$(exiftool -T -UpdatedTitle "$1") - $(exiftool -T -Author "$1")"
    #An alternative that works with EPUBs, too:
    # ebook-meta epub|grep -i title|read -r a a b; echo $b

    echo mv "$1" "${directory}/${newfilename}.${extension}"
    mv "$1" "${directory}/${newfilename}.${extension}"
}
function cpt() { echo -n "$@" | pbcopy; }
function sdc() {
    it2prof dark
    sdcv --color "$*" | less
    it2prof 'Hotkey Window'
}
function sp() { ispell<<<"$*" ; }
function fsay() {
    say -v Fiona -r 30 "$@"
    #PORTME
}
function months() {
    echo "1.  January - 31 days
2.  February - 28 days in a common year and 29 days in leap years
3.  March - 31 days
4.  April - 30 days
5.  May - 31 days
6.  June - 30 days
7.  July - 31 days
8.  August - 31 days
9.  September - 30 days
10. October - 31 days
11. November - 30 days
12. December - 31 days"
}
function xlad(){
    last-added|xargs -I _ "$@"
}
