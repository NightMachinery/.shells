aga() {
    ag "$@" ~/scripts/**/alias*
}
agf() {
    ag "$@" ~/scripts/**/functions*
}
ags() {
    ag "$@" ~/.zshenv ~/.zshrc ~/scripts/**/*
}
imdb() imdbpy search movie --first "$*"
playtmp() {
    mkdir -p ~/tmp/delme/
    cp "$1" ~/tmp/delme/
    color 0 200 0 Copied "$1" to tmp
    fsay Copied to tmp
    pat ~/tmp/delme/"$1:t"
}
mut() {
    music_dir=$HOME'/Downloads/Telegram Desktop' songc --loop "$*"
}
muu() songc --loop "$*"
mub() {
    songc --loop-playlist "$*" #alBum
}
mup() playlistc "$@"
mud() {
    local bp=()
    { test "${1}" = "-d" || test "$1" = "-b" || test "$1" = "-p" } && {
        bp+="$1"
        shift
    }
    songd "$bp[@]" --loop-playlist "$*" #Download
}
svpl() {
    # Save Playlist save-playlist save-pl
    mv "$(last-created "${playlist_dir:-$HOME/playlists}/autopl")" "${playlist_dir:-$HOME/playlists}/$1"
}
hearp() {
    local shuf='--shuffle'
    test "${1}" = '+s' && {
        shuf=''
        shift
    }
    local tracks="$(map '$1
' "$(cat "${@}")")"
    test -z "$shuf" || tracks="$(ec "$tracks"|shuf)"
    test -z "$NO_HEARP_TOUCH" && {
        touch-tracks "${(@f)tracks}"
    }
    # Don't use mpv's native --shuffle since it MIGHT use autoloaded tracks, also empty string causes a harmless error
    # k shuffles live in mpv (with MY config :D)
    hear --loop-playlist --playlist <(ec "$tracks")
}
trs() {
    re 'ec Trying to remove' "$@"
    trash -- "$@"
}
songc() {
    # Please note that I am relying on the auto-load plugin of mpv to load all files in a folder. If you don't have that, remove the `-e EXT` filters of fd in this function.
    local p="${@: -1}"
    test -z "${p##-*}" && set -- "$@" '.'
    test -z "${p##-*}" && p='.'
    # ec "$p" "${@:0:-1}"
    # test -z "$p" && set "$@"
    local f
    f=()
    local f2="$(playlister "$p")"
    f+=( ${(@f)f2} )
    # zsh is messed up with its arrays
    local autopl="${playlist_dir:-$HOME/playlists}/autopl/"
    mkdir -p "$autopl"
    gfind "$autopl" -mindepth 1 -type f -mtime +3 -delete
    # ec $#f
    test $#f -gt 1 && ec "$f2" > "$autopl/$(date)"
    ! test -z "$f" && { touch-tracks  "${(@f)f}" ; hear "${@:1:-1}" "${(@f)f}" }
}
touch-tracks() {
    local track
    for track in "$@"
    do
        test -e "$track" && touch "$track" #"$(bottomdir "$track")"
    done
}
playlistc() {
    local pl="$(fd --follow -t f '.' "${playlist_dir:-$HOME/playlists/}" | fz -q "$*")"
    test -z "$pl" || { ec "Playing playlist(s) $pl" && hearp "${(@f)pl}" }
}
playlister() {
    find-music "$@" | fz --history "$music_dir/.fzfhist" # -q "$1"
}
find-music() {
    fd -c never --follow -e m4a -e mp3 -e flac "$*" "${music_dir:-$HOME/my-music}"
}
songd() {
    doc 'zsh-only
    Use songc to play already downloaded files.
    Set PRUNE_SONGD_DAYS to, e.g., +120 to remove files older (measured by access time) than 120 days from the cache.'
    ecdbg "$@"
    local music_dir="${music_dir:=$HOME/my-music/}/${musiccache:-cache}"
    local musiccache='/' #To avoid recursion. Can't be empty or it'll auto replace.
    mkdir -p "$music_dir"
    test -z "$PRUNE_SONGD_DAYS" || {
        gfind "$music_dir" -mindepth 1 -type f -mtime "$PRUNE_SONGD_DAYS" -print -delete >> "$logdir"/prune_songd 2>&1 | tee
        # Access time itself is hard to use
        # https://unix.stackexchange.com/questions/530896/removing-directories-not-accessed-in-x-days
        # So we TOUCH :')
    }
    silence eval '\rm -r -- "$music_dir/"*(-@D)' #The characters in parentheses are glob qualifiers: - to dereference symlinks, @ to match only symlinks (the combination -@ means broken symlinks only), and D to match dot files. To recurse into subdirectories, make that rm -- **/*(-@D).
    local bp
    { test "${1}" = "-d" || test "$1" = "-b" || test "$1" = "-p" } && {
        bp="$1"
        shift
    }
    ecdbg "$@"
    local q="${@: -1}"
    local spath="$music_dir/${q:gs#/# }/"
    ecdbg "spath: $spath"
    ecdbg "real spath:"  "$(realpath "$spath")"
    test "$bp" = "-d" && {
        trs "$(realpath "$spath")"
        trs "$spath"
        (exit 0)
    } || {
        test -e "$spath" && {
            ecdbg Cache found
            touch "$(bottomdir "$spath")"
            eval 'touch-tracks "$spath"/*' || {
                ecdbg Could not TOUCH tracks, probably no tracks are available.
                trs "$spath"
                (exit 1)
            } && {
                hear "${@:1:-1}" "$spath"
                (exit 0) }
        } || {
            local usedCache=''
            [[ "$q" =~ "^http" ]] || {
                # colorbg 0 0 255
                # colorfg 0 255 0
                # find-music "$q" #Printing available music
                comment 'the -1 autoselect feature of fzf can cause false positives but you can then just interrupt mpv which will make it exit non-zero.'
                comment "songc currently feeds the query into fd, so it's not fuzzy."
                songc "$@" && usedCache='y' && return 0
                # I just learned about `return`, so the usedCache logic is now useless. It's also kind of buggy and returns 1 in case of success.
                # resetcolor
            }
            ecdbg "usedCache: $us"
            test -z "$usedCache" && {
                #nonexistent path
                fsaydbg Cache NOT found
                mkdir -p "$spath"
                test -z "$bp" && {
                    spotdl -f "$spath" -s "$q" && { sleep 1 && songd "$@" } || {
                        songd -d "$@"
                    }
                } || {
                    local bp_name
                    spotdl_dir="$spath" aget sdlg "$bp" "$q:q" '&& bp_name=(./**/*.txt) ' && {
                        local bp_path="$music_dir/${bp_name:t:r:gs/[-_]/ }"
                        mkdir -p "$bp_path"
                        mv "$spath"/*(D) "$bp_path"
                        \rm -r "$spath"
                        ln -s "$bp_path" "$(removeTrailingSlashes "$spath")"
                        sleep 1 && songd "$bp" "$@"
                    } || {
                        songd -d "$@"
                    }
                }
            }
        }
    }
}
killjobs () {
    local kill_list="$(jobs)"
    if [ -n "$kill_list" ]; then
        # this runs the shell builtin kill, not unix kill, otherwise jobspecs cannot be killed
        # the `$@` list must not be quoted to allow one to pass any number parameters into the kill
        # the kill list must not be quoted to allow the shell builtin kill to recognise them as jobspec parameters
        kill $@ $(gsed --regexp-extended --quiet 's/\[([[:digit:]]+)\].*/%\1/gp' <<< "$kill_list" | tr '\n' ' ')
    else
        return 0
    fi
}
redo() {
    local i
    for i in {1.."${@: -1}"}
    do
        eval "${@: 1:-1:q}"
    done
}
ks () { kscript ~/kscripts/"$@"; }

cdm ()
{
    mkdir -p -- "$1" &&
        cd -P -- "$1"
}


function bottomdir () {
    [ -f "$1" ] && { ec "$(dirname "$1")"; } || { ec "$1"; } ;}
function cdd () {
    cd "$(bottomdir "$1")" }

transfer() {
    #
    # Defines transfer alias and provides easy command line file and folder sharing.
    #
    # Authors:
    #   Remco Verhoef <remco@dutchcoders.io>
    #
    # check arguments
    if [ $# -eq 0 ];
    then
        echo "No arguments specified. Usage:\necho transfer /tmp/test.md\ncat /tmp/test.md | transfer test.md"
        return 1
    fi

    # get temporarily filename, output is written to this file show progress can be showed
    tmpfile=$( mktemp -t transferXXX )

    # upload stdin or file
    file=$1

    if tty -s;
    then
        basefile=$(basename "$file" | sed -e 's/[^a-zA-Z0-9._-]/-/g')

        if [ ! -e $file ];
        then
            echo "File $file doesn't exists."
            return 1
        fi

        if [ -d $file ];
        then
            # zip directory and transfer
            zipfile=$( mktemp -t transferXXX.zip )
            cd $(dirname $file) && zip -r -q - $(basename $file) >> $zipfile
            curl --progress-bar --upload-file "$zipfile" "https://transfer.sh/$basefile.zip" >> $tmpfile
            rm -f $zipfile
        else
            # transfer file
            curl --progress-bar --upload-file "$file" "https://transfer.sh/$basefile" >> $tmpfile
        fi
    else
        # transfer pipe
        curl --progress-bar --upload-file "-" "https://transfer.sh/$file" >> $tmpfile
    fi

    # cat output link
    cat $tmpfile

    # cleanup
    rm -f $tmpfile
}

function git_sparse_clone() (
    # git_sparse_clone "http://github.com/tj/n" "./local/location" "/bin"
    rurl="$1" localdir="$2" && shift 2

    mkdir -p "$localdir"
    cd "$localdir"

    git init
    git remote add -f origin "$rurl"

    git config core.sparseCheckout true

    # Loops over remaining args
    local i
    for i; do
        echo "$i" >> .git/info/sparse-checkout
    done

    git pull origin master
)

function rloop_vid() (
    ffmpeg -i "$1" -filter_complex "[0:v]reverse,fifo[r];[0:v][r] concat=n=2:v=1 [v]" -map "[v]" "$1_rloop.${2:-mp4}"
)

function trr() (
    peerflix "$@" --path "${PEERFLIX_DIR:-$HOME/Downloads/Video}" --mpv -- --fullscreen
)
function ot-mp3() (
    B=$(basename "$1"); D=$(dirname "$1");
    ffmpeg -ss 1.5 -i "$1" -metadata artist="Our Apparitions" -metadata title="${B%.*}" -codec:a libmp3lame -qscale:a 1 "$D/${B%.*}.mp3" "${@:2}"
    ffmpeg -ss 1.5 -i "$1" -metadata artist="Our Apparitions" -metadata title="${B%.*}" -codec:a copy "$D/${B%.*}.trimmed.wav" "${@:2}"
)
function ot-wav() {
    B=$(basename "$1"); D=$(dirname "$1");
    ffmpeg -ss 1.5 -i "$1" -metadata artist="Our Apparitions" -metadata title="${B%.*}" -codec:a copy "$D/${B%.*}.trimmed.wav" "${@:2}"
}

function mp3-to-mp4() (
    B=$(basename "$1"); D=$(dirname "$1");
    ffmpeg -loop 1 -i "$2" -i "$1" -pix_fmt yuv420p -c:v libx264 -crf 16  -c:a libfdk_aac -vbr 5 -preset veryslow -vf pad="width=ceil(iw/2)*2:height=ceil(ih/2)*2:x=0:y=0:color=black" -shortest "${3:-$D/${B%.*}}.mp4"
    # -c:a copy -r 1
)
function sleepnow() ( sleep "${1:-7}"; pmset sleepnow )
silent_background() {
    { 1>/dev/null 2>&1 3>&1 eval "$@"& }
    disown &>/dev/null  # Prevent whine if job has already completed
}
function rm-alpha() {
    local B=$(basename "$1"); local D=$(dirname "$1");
    convert "$1" -background "$2" -alpha remove "$D/${B%.*}_$2.png"
}
function alpha2black() (rm-alpha "$1" black)
function alpha2white() (rm-alpha "$1" white)

combine-funcs alpha2bw alpha2black alpha2white
function hi10-multilink() {
    #zsh-only
    local argCount=$#
    local pArgs=()
    local i
    for (( i=1; i<=$argCount; i+=1 ))
    do
        if [[ "$argv[i]" =~ '.*http:\/\/hi10anime(.*)' ]]; then #'.*http:\/\/ouo.io\/s\/166CefdX\?s=(.*)' ]]; then
            # echo $match[1]
            pArgs[$i]='http://hi10anime'"$match[1]"
        else
            echo Invalid link: "$argv[i]"
        fi
    done
    # echo $pArgs
    # --referer="$1" is not needed now, if needed be sure to use regex matching to give it, as the urls returned from lynx are invalid.
    aria2c -j1 -Z  "${(@u)pArgs}" # (u) makes the array elements unique.
}
function hi10-from-page() {
    # You need to have persistent cookies in lynx, and have logged in.
    hi10-multilink "${(@f)$(lynx -cfg=~/.lynx.cfg -cache=0 -dump -listonly $1|grep -E -i ${2:-'.*\.mkv$'})}"
    # eval 'hi10-multilink ${(@f)$(lynx -cfg=~/.lynx.cfg -cache=0 -dump -listonly "'"$1"'"|grep -E -i "'"${2:-.*\.mkv$}"'")}'
}
function ppgrep() {
    case "$(uname)" in
        Darwin)
            \pgrep -i "$@" | gxargs --no-run-if-empty ps -fp
            ;;
        Linux)
            \pgrep "$@" | gxargs --no-run-if-empty ps -fp
            # Linux's pgrep doesn't support -i
            ;;
    esac
}
function '$'() { eval "$@" ; }

function timer-raw() {
    #aliased to timer with noglob
    eval "sleep $((($1)*60))" && eval "${@:2:q}"
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
    case $1 in
        *[!/]*/) ec "$1"|sed 's:/*$::' ;; #x=${x%"${x##*[!/]}"};;
        *[/]) ec "/";;
    esac
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
    [[ "$1" =~ 'mobi.az1$' ]] && {
        mv "$1" "${1:r}"
        ecdbg "az1 detected; Renaming to ${1:r}"
        set -- "${1:r}"
    } || if test "${1:e}" != mobi ; then 2mobi "$1"
                                         set -- "${1:r}.mobi"
    fi
    2kindle "$1"
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
    eval-dl "brew install $1" "sudo apt install -y $1"
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
function onla() {
    geval "$@:q"" ${$(last-added):q}"
}
function onxla(){
    last-added|xargs -I _ "$@:q"
}
function first-file(){
    exa|head -n1
}
function onlac(){
    geval "$@:q"" ${$(last-accessed):q}"
}
function onlm(){
    geval "$@:q"" ${$(last-modified):q}"
}
function onlc(){
    geval "$@:q"" ${$(last-created):q}"
}
function onff(){
    geval "$@:q"" ${$(first-file):q}"
}
function play-and-trash(){
    #aliased to pat
    mpv "$@" && trs "$1"
}
url-final() {
    curl -Ls -o /dev/null -w %{url_effective} "$@"
}
url-tail() {
    [[ "$1" =~ '\/([^\/]+)\/?$' ]] && ec "$match[1]"
}
function tlrlu(){
    tlrl "$@" -p "$(url-tail "$(url-final "$1")") | "
}

function raise-blood() ceer rederr.zsh source
function rp() test -e "$1" && realpath "$1" || ceer "$1" realpath
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
    setopt local_options pipefail
    local title author
    test "${2:=markdown}" = 'html' && title='"<h1>"+.title+"</h1>"' || title='"# "+.title'
    test "${2}" = 'html' && author='"<p>By: <b>"+.author+"</b></p>"' || author='"By: **"+.author+"**"'
    mercury-parser --format="${2}" "$1" |jq -e --raw-output '[
    (if .title then '"$title"' else empty end),
    (if .author then '"$author"' else empty end),
    .content
] | join("\n\n")' #'.content'
}
function random-poemist() {
    curl -s https://www.poemist.com/api/v1/randompoems |jq --raw-output '.[0].content'
}
xkcd() wget `wget -qO- dynamic.xkcd.com/comic/random | sed -n 's/Image URL.*: *\(\(https\?:\/\/\)\?\([\da-z\.-]\+\)\.\([a-z\.]\{2,6\}\)\([\/\w_\.-]*\)*\/\?\)/\1/p'`
les() { eval "$@:q" |& less }
lesh() les "$1" --help
html2epub-calibre() {
    local u="$1 $(uuidgen).html"
    merge-html "${@:3}" > "$u"
    ebook-convert "$u" "$1.epub" \
                  --authors="$2" \
                  --level1-toc="//*[name()='h1' or name()='h2']" \
                  --level2-toc="//h:h3" \
                  --level3-toc="//*[@class='subsection']" \
                  --page-breaks-before="//*[(name()='h1' or name()='h2') or @class='owner-name']" \
                  --use-auto-toc --toc-threshold=0 \
                  --toc-title="The TOC" \
                  --embed-all-fonts \
                  --title="$1" --epub-inline-toc
    \rm "$u"
}
merge-html() {
    map '

 <h1>$(strip $1 ".html")</h1>

 $(cat $1)' "$@"
}
html2epub() {
    "${h2ed:-html2epub-calibre}" "$@"
}
html2epub-pandoc() {
    # title author htmls
    pandoc --toc -s -f html <(merge-html "${@:3}") --epub-metadata <(ec "<dc:title>$1</dc:title> <dc:creator> $2 </dc:creator>") -o "$1.epub"
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
                              mv *.epub ../ && cd '../' && \rm -r "./$u"
                              ec "Book '$1' by '$2' has been converted successfully."
    } || { ecerr "$hasFailed" && (exit 1) }
}
w2e() {
    web2epub "$1" "nIght is long and lonely" "${@:2}" && 2m2k "$1.epub"
}
emn() {
    emc -e "(woman \"$*\")"
}
swap-audio() {
    ffmpeg -i "$1" -i "$2" -c:v copy -map 0:v:0 -map 1:a:0 -shortest "$3"
}
function tsox() {
    silence ffmpeg -i "$1" "${1:r}".wav && sox "${1:r}".wav "${1:r}.$2" -G "${@:3}"
}
function vdsox() {
    local inp=(*)
    tsox "$inp" '2.wav' "$@" && silence swap-audio "$inp" "${inp:r}.2.wav" "${inp:r}.c.${inp:e}" && \rm -- ^*.c.${inp:e}
    silence jvideo
}
function vasox() {
    local inp=(*)
    tsox "$inp" 'c.mp3' "$@"
    \rm -- ^*.mp3
}
function vosox() {
    opusdec --force-wav * - 2> /dev/null | sox - "brave_n_failed.mp3" -G "$@"
    silence jopus
}
function vsox() {
    local inp=(*)
    sox "$inp" "${inp:r}_c.mp3" -G "$@"
}
function sdl() {
    local bp
    { test "${1[1]}" = "-" } && { # || test "$1" = "-b" || test "$1" = "-p" } && {
        bp="$1"
        shift
    }
    test -z "$bp" && {
        nisout spotdl -f "${spotdl_dir:-.}" -s "$*" } || {
        nisout sdlg "$bp" "$@"
    }
}
function pdf-cover() {
    convert "$1[0]" "$1:r.png"
}
function sdlg() {
    #use with aget
    spotdl "$@" && spotdl -f "${spotdl_dir:-.}"  -l *.txt && {
            mkdir -p ./ghosts/
            mv *.txt ./ghosts/
        }
}
function aget() {
    local u="$(uuidgen)"
    local err
    cdm "$u"
    eval "$@" && {
        cd ..
        \rm -r "$u"
    } || { err="$?" && ecerr aget "$@" exited "$err"; l ; cd .. ; (exit "$err") }
}
function jsummon() {
    mkdir -p ~/julia_tmp/
    local u=(*)
    mv "$u" ~/julia_tmp/
    realpath ~/julia_tmp/"$u"
}
function junsummon() {
    \rm -r ~/julia_tmp
}
function prefix-files() {
    for file in "${@:2}"
    do
        mv "$file" "${file:h}/$1${file:t}"
    done
}
function jpre() {
    jrm
    eval "prefix-files $1:q ${jpredicate:-*(D.)}"
}
function jvoice() {
    jpre "voicenote-"
}
jvideo() jpre "videonote-"
jdoc() jpre "fdoc-"
jstream() jpre "streaming-"
jmv() {
    test -e "$jufile" && mv "$jufile" "n_$jufile"
}
jrm() {
    test -e "$jufile" && \rm "$jufile"
}
jopus() {
    jrm
    local u=(*)
    ffmpeg -i "$u" -strict -2 "${u:r}.opus"
    \rm "$u"
    jvoice #actually unnecessary as Telegram sees most (size threshold probably) opus audio as voice messages:))
}
insadd() {
    ec "$1" >> "$insables"
    ins "$1"
}
clean-dups() {
    sort -u "$1" | sponge "$1"
}
clean-insables() clean-dups "$insables"
bnu() {
    # brew-no-update
    export HOMEBREW_NO_AUTO_UPDATE=1
}
jclosh() clojure -Sdeps '{:deps {closh {:git/url "https://github.com/dundalek/closh.git" :tag "v0.4.0" :sha "17e62d5bceaa0cb65476e00d10a239a1017ec5b8"}}}' -m closh.zero.frontend.rebel
