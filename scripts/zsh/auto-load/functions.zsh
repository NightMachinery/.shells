wh() { which "$@" |btz }
rtf2txt() { unrtf "$@" | html2text }
mn() {
    man "$@" || lesh "$@"
}
blc() {
    doc brew link custom
    mkdir -p ~/bin/
    ln -s "$(brew --cellar "$1")"/**/"$2" ~/bin/"$3"
}
function github-dir() {
    svn export "$(sed 's/tree\/master/trunk/' <<< "$1")" "$2"  
}
function pbadd() {
    osascript "$NIGHTDIR"'/applescript/path-copy.applescript' "${(f)$(re realpath $@)}" > /dev/null
}
function ls-by-added() {
    # Doesn't work well for files having '(null)' as their DateAdded, which some do.
    mdls -name kMDItemFSName -name kMDItemDateAdded -raw -- *(D) | \
        xargs -0 -I {} echo {} | \
        sed 'N;s/\n/ /' | \
        sort --reverse | \
        sed -E "s/^.*\\+0000 //" # removes the timestamps
}
# increment-episode() {
# superseded by tmnte
#   emulate -L zsh
#   setopt extendedglob
#   local cmd=${$(fc -nl -1 -1)/(#b)(*E)(<->)/$match[1]${(l:${#match[2]}::0:)$((match[2]+${1:-1}))}}
#   geval "$cmd"
# }
imdb() imdbpy search movie --first "$*"
playtmp() {
    mkdir -p ~/tmp/delme/
    cp "$1" ~/tmp/delme/
    color 0 200 0 Copied "$1" to tmp
    fsay Copied to tmp
    pat ~/tmp/delme/"$1:t"
}
killjobs() {
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
ks() { kscript ~/kscripts/"$@"; }

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
function '$'() { eval "$(gquote "$@")" ; }

function timer-raw() {
    doc aliased to timer with noglob
    eval "sleep $((($1)*60))" && eval ${(q+@)@[2,-1]:-${(z)/#/loop fsayd}}
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
        *[!/]*) ec "$1"|sed 's:/*$::' ;; #x=${x%"${x##*[!/]}"};;
        [/]*) ec "/";;
    esac
}
function whz() {
    printz "$(which "$1")" #"${(q-@)"$(which "$1")"}"
}
function k2pdf() {
    nis k2pdfopt "$@" -dev kv -png -bpc 2 -d -wrap+ -hy- -ws -0.2 -x -odpi "${k2_odpi:-450}" -y -ui-
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
    local e i
    e=( "()" "{" "}" '"$@"' ':' aliased to noglob run-on-each ruu proxychains4 'DEBUGME=d' ) 
    i=( "${=$(which "$@")}" )
    wh "${(@)i[2,-1]:|e}"
}
function cee() {
    cat `which "$1"`
}
function ceer() {
    geval "${@:2} $(which "$1")"
}
function setv() {
    doc macOnly
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
    retry-limited-eval "$1" "$(gquote "${@:2}")"
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
function ncp() {
    cat | gnc -c localhost 2000
}
it2prof() { echo -e "\033]50;SetProfile=$1\a" ; } # Change iterm2 profile. Usage it2prof ProfileName (case sensitive)
function aas() {
    # aa "$@" --on-download-start aa-stream
    local out="$(md5 <<<"$1")"
    aa "$@" --dir "$out" --on-download-complete aa-stream &
    retry-mpv "'$out'/*" || kill %
}
function y-stream() {
    y -f best  -o "%(title)s.%(ext)s" "$@" &
    local out=$(yic -f best --get-filename -o "%(title)s.%(ext)s" "$@")
    #We need to use yic or archived videos return nothing causing mpv to play * :D
    test -n "$out" && retry-mpv "${out:q}*" || ecerr Could not get video\'s name. Aborting.
    #mpv bug here
    # kill $!
    # kill $! is your friend :))
}
function retry-mpv() {
    retry-eval "mpv --quiet $@ |& tr '\n' ' ' |ggrep -v 'Errors when loading file'"
}
function set-fk-icon-size() {
    /usr/libexec/PlistBuddy -c "set FK_DefaultIconViewSettings:iconSize ${1:-128}" ~/Library/Preferences/com.apple.finder.plist # This is for Finderkit, i.e., dialogs.
}
function set-finder-icon-size() {
    /usr/libexec/PlistBuddy -c "set StandardViewSettings:IconViewSettings:iconSize ${1:-128}" ~/Library/Preferences/com.apple.finder.plist # This is for Finder itself.
}
function cpt() { echo -n "$@" | pbcopy; }
