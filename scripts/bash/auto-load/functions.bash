function timer-raw() {
    #aliased to timer with noglob
    eval "sleep $((($@)*60)) && loop 1 fsayd"
}
function ubuntu-upgrade() {
    sudo apt update 
    sudo apt upgrade
    sudo apt dist-upgrade
}
function zir() {
    local dest="$(removeTrailingSlashes "$1")$2".zip
    \rm "$dest" &> /dev/null
    zip -r "$dest" "$1"
}
function removeTrailingSlashes() {
    echo "$1"|sed 's:/*$::'
}
function p() {
    geval "$@" ${"$(pbpaste)":q}
}
function k2pdf() {
    k2pdfopt "$@" -dev kv -png -bpc 2 -d -wrap+ -hy- -ws -0.2 -x -odpi 450 -y -ui-
    # -as 
}
function display-off() {
    watch -n ${1:-1} brightness 0
    #macOS only probably
}
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
function ceer() {
	geval "${@:2} $(which "$1")"
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
    retry-limited-eval 0 "$@"
	  # until eval "$@" ; do
# 		    echo Retrying \'"$*"\' "..." 1>&2
# 		    sleep 1
# 	  done
}

function retry-limited() {
    retry-limited-eval "$1" "${@:2:q}"
}
function retry-limited-eval() {
    local limit=0
    local ecode=0
	  until {test "$1" -gt 0 && test $limit -ge "$1"} || { eval "${@:2}" && ecode=0 }
    do
        ecode="$?"
		    ecerr Tried eval "${@:2}" "..."
		    sleep 1
        limit=$((limit+1))
	  done
    # test $limit -lt "$1" || test "$1" -eq 0
    (exit "$ecode")
}
function 2mobi() {
	ebook-convert "$1" "${1:r}.mobi"
}
function 2m2k() {
	if test "${1:e}" != mobi ; then 2mobi "$1" ; fi
	2kindle "${1:r}.mobi"
}
function aap() {
	  aa "$@" --on-download-complete aa-pToKindle
}
function aab() {
	aa "$@" --on-download-complete aa-toKindle
}
function 2kindle() {
	mutt -s "${2:-convert}" -a "$1" -- "${3:-fifya@kindle.com}" <<<hi
}
function 2ko() {
    2kindle "$1" "some_subject" "$2"
}
function 2p2k() {
    k2pdf "$1"
    2ko "${1:r}_k2opt.pdf"
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
function aas() {
	  # aa "$@" --on-download-start aa-stream
    local out="$(uuidgen)"
    aa "$@" --dir "$out" --on-download-complete aa-stream &
    retry-mpv "'$out'/*"
}
function y-stream() {
    y -f best  -o "%(title)s.%(ext)s" "$@" &
    local out=$(yic -f best --get-filename -o "%(title)s.%(ext)s" "$@")
    #We need to use yic or archived videos return nothing causing mpv to play * :D
    retry-mpv "${out:q}*"
    #mpv bug here
    # kill $!
    # kill $! is your friend :))
}
function retry-mpv() {
    retry-eval "mpv --quiet $@ |& tr '\n' ' ' |ggrep -v 'Errors when loading file'"
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
function lad() {
    eval "$@"" ${$(last-added):q}"
}
function xlad(){
    last-added|xargs -I _ "$@"
}
function first-file(){
    exa|head -n1
}
function las(){
    eval "$@"" ${$(first-file):q}"
}
function play-and-trash(){
    #aliased to pat
    mpv "$@" && trs "$1"
}
function tlrlu(){
	tlrl "$@" -p "$1   "
}
function rexx(){
	xargs -d " " -n 1 -I _ "$=1" <<< "${@:2}"
}
function rex(){
        zargs --verbose -i _ -- "${@:2}" -- "$=1"
	#Using -n 1 fails somehow. Probably a zargs bug.
}
function rexa(){
	local i
        for i in "${@:2}"
        do
		eval "$(sed -e "s/_/${i:q:q}/g" <<< "$1")" #sed itself needs escaping, hence the double :q; I don't know if this works well.
        done
}
function tel(){
    "${@:2}" "$(which "$1")"
}
function expand-alias {
    if [[ -n $ZSH_VERSION ]]; then
        # shellcheck disable=2154  # aliases referenced but not assigned
        printf '%s\n' "${aliases[$1]}"
    else  # bash
        printf '%s\n' "${BASH_ALIASES[$1]}"
    fi
}
function force-expand {
    local e="$(expand-alias "$1")"
    test -z "$e" && e="$1"
    echo "$e"
}
function ruu() {
    local a="$(force-expand "$2")"
    a="$(strip "$a" 'noglob ')"
    "$1" "$=a" "${@:3}"
}
function geval() {
    local cmd="$@"
    ec "$cmd"
    print -r -S -- "$cmd" #Add to history
    eval -- "$cmd"
}
function ec() {
    if [[ -n $ZSH_VERSION ]]; then
    print -r "$@"
    else  # bash
    echo -E "$@"
    fi
}
function rederr() {
	(setopt nomultios 2>/dev/null; set -o pipefail; eval "$@:q" 2>&1 1>&3|sed $'s,.*,\e[31m&\e[m,'1>&2)3>&1
}
ecerr() ec "$@" 1>&2
function raise-blood() ceer rederr.zsh source
increment-last () {
    #$1 is supplied in our alias tmnt. :D
    local pe='s/'$1'/$1 . (sprintf "%0*d", length($2), $2 + '"${2:-1}"')/e'
    #ec "$pe"
    #fc might not work in bash
    local cmd=${$(fc -nl -1 -1| perl -pe "$pe")}
    geval "$cmd"
}
function away() {
    nohup "$@" & disown
}
function wt1() {
	curl -s 'wttr.in/{'"${1:-Tehran,Sabzevar,Kish,Mashhad,نمک‌آبرود,اردبیل}"'}?format="%l:+%C+%c+%t+%h+%w+%m+%M+%p"&m'
}
function wread() {
    (
        set -o pipefail
	      mercury-parser --format="${2:-markdown}" "$1" |jq -e --raw-output '.content'
    )
}
function random-poemist() {
	curl -s https://www.poemist.com/api/v1/randompoems |jq --raw-output '.[0].content'
}
xkcd() wget `wget -qO- dynamic.xkcd.com/comic/random | sed -n 's/Image URL.*: *\(\(https\?:\/\/\)\?\([\da-z\.-]\+\)\.\([a-z\.]\{2,6\}\)\([\/\w_\.-]*\)*\/\?\)/\1/p'`
les() { eval "$@:q" |& less }
lesh() les "$1" --help
html2epub() {
    # title author htmls
    pandoc --toc -s -f html <(map '

 <h1>$(strip $1 ".html")</h1>

 $(cat $1)' "${@:3}") --epub-metadata <(ec "<dc:title>$1</dc:title> <dc:creator> $2 </dc:creator>") -o "$1.epub"
} 
h2e() html2epub "$1" "nIght is long and lonely" "${@:2}"
web2epub() {
    # title author urls-in-order
    local u="$1 $(uuidgen)"
    cdm "$u"

    local i=0
    local hasFailed=''
    for url in "${@:3}"
    do
        local bname="${url##*/}"
        #test -z "$bname" && bname="u$i"
        bname="${(l(${##})(0))i} $bname.html"
        i=$((i+1))

        retry-limited-eval "${W2E_RETRY:-10}" wread "$url:q" html '>' "$bname:q" && ec "Downloaded $url ..." || { ec "$url" >> failed_urls
                                                                                        ecerr "Failed $url"
                                                                                        hasFailed='Some urls failed (stored in failed_urls). Download them yourself and create the epub manually.'
        }
    done

    test -z "$hasFailed" && { ec "Converting to epub ..."
                              html2epub "$1" "$2" *.html
                              mv *.epub ../
                              ec "Book '$1' by '$2' has been converted successfully."
                              cd '../'
                              \rm -r "./$u" } || { ecerr "$hasFailed" && (exit 1) }
}
w2e() {
    web2epub "$1" "nIght is long and lonely" "${@:2}" && 2m2k "$1.epub"
}
emn() {
    emc -e "(woman \"$*\")"
}
swap-audio() {
    ffmpeg -i "$1" -i "$2" -c:v copy -map 0:v:0 -map 1:a:0 "$3"
}

function vdsox() {
    ffmpeg -i * o.wav && sox o.wav d.wav -G "$@" && swap-audio *.mp4 d.wav o.mp4 && \rm ^*.mp4
}
