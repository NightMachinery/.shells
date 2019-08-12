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
function rename-for-podcast0() {
    local c=1
    local i
    for i in "${@:3}"
             {
                 echo "$i to "  "$(dirname $i)/1-$(printf '%02d' $c) $2"
                 if test $1 = 1 ; then mv "$i" "$(dirname $i)/1-$(printf '%02d' $c) $2"; fi
                 c=$(($c + 1))
             }
}
function pbadd() {
    osascript ~/'scripts/applescript/path-copy.applescript' "${(f)$(re realpath $@)}" > /dev/null
}
function cpsd() {
    local i;
    local counter=1;
    local outs=();
    for i in "${@:2}"; do
        local B=$(basename "$i"); #local D=$(dirname "$i");
        local out="$1/${B%.*}.png"
        convert "$i""[0]" "$out"
        outs[$counter]="$out"
        printf '%s\n' "$out"
        counter=$(($counter + 1))
    done
    pbadd "${(@)outs}"
}
function cpsdi() {
    cpsd "$(dirname "$1")" "$@"
}
function cpsdt() {
    mkdir -p ~/tmp/delme
    cpsd ~/tmp/delme "$@"
}
function psd2telg() {
    cpsdt "$@"|xargs -I {} ensure-run.zsh 60s tsend me ' ' -f {} --force-document
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
sin() {
    export FORCE_INTERACTIVE=y
    sb
    source ~/.zshrc
    eval "$(gquote "$@")"
}
imdb() imdbpy search movie --first "$*"
playtmp() {
    mkdir -p ~/tmp/delme/
    cp "$1" ~/tmp/delme/
    color 0 200 0 Copied "$1" to tmp
    fsay Copied to tmp
    pat ~/tmp/delme/"$1:t"
}
trs() {
    re 'ec Trying to remove' "$@"
    trash -- "$@"
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
        *[!/]*/) ec "$1"|sed 's:/*$::' ;; #x=${x%"${x##*[!/]}"};;
        *[/]) ec "/";;
    esac
}
function p() {
    geval "$(gq "${@}")" "${"$(pbpaste)":q}"
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
    wh $(strip "`wh "$1"`" "$1: aliased to ")
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
function frep() {
    doc '@convenience @grep'
    eval "${@:2:q}" |& ggrep -iP "$1"
}
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
function loop() { while true; do eval "${@}"; sleep ${lo_s:-1}; done ; }
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
    geval "$(gquote "$@")"" ${$(last-added):q}"
}
function onxla(){
    last-added|gxargs -I _ "$(gquote "$@")"
}
function onxlc(){
    last-created|gxargs -I _ "$(gquote "$@")"
}
function first-file(){
    exa|head -n1
}
function onlac(){
    geval "$(gquote "$@")"" ${$(last-accessed):q}"
}
function onlm(){
    geval "$(gquote "$@")"" ${$(last-modified):q}"
}
function onlc(){
    geval "$(gquote "$@")"" ${$(last-created):q}"
}
function onff(){
    geval "$(gquote "$@")"" ${$(first-file):q}"
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
function rp() {
    test -e "$1" && realpath "$1" || ge_no_ec=y ge_no_hist=y ceer "$1" realpath
}
increment-last() {
    #$1 is supplied in our alias tmnt. :D
    local pe='s/'$1'/$1 . (sprintf "%0*d", length($2), $2 + '"${2:-1}"')/e'
    #ec "$pe"
    #fc might not work in bash
    local cmd=${$(fc -nl -1 -1| perl -pe "$pe")}
    geval "$cmd"
}
outlinify() {
    map 'https://outline.com/$1' "$@"
}
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
                  --title="$1" --epub-inline-toc --enable-heuristics
    \rm "$u"
}
merge-html() {
    map '

 <h1>$(strip $1 ".html")</h1>

 $(cat $1)' "$@"
}
html2epub() {
    ecdbg calling "${h2ed:-html2epub-calibre}" "$@"
    "${h2ed:-html2epub-calibre}" "$@"
}
html2epub-pandoc() {
    # title author htmls
    pandoc --toc -s -f html <(merge-html "${@:3}") --epub-metadata <(ec "<dc:title>$1</dc:title> <dc:creator> $2 </dc:creator>") -o "$1.epub"
}
h2e() html2epub "$1" "${h2_author:-night}" "${@:2}"
web2epub() {
    doc usage: 'we_retry= we_dler= we_author= title urls-in-order'
    local u="$1 $(uuidgen)"
    cdm "$u"
    local author="${we_author:-night}"
    local i=0
    local hasFailed=''
    for url in "${@:2}"
    do
        local bname="${url##*/}"
        #test -z "$bname" && bname="u$i"
        bname="${(l(${##})(0))i} $bname"
        i=$((i+1))

        retry-limited-eval "${we_retry:-10}" "${we_dler:-wread}" "$url:q" html '>' "$bname:q" && ec "Downloaded $url ..." || { ec "$url" >> failed_urls
                                                                                                                               ecerr "Failed $url"
                                                                                                                               hasFailed='Some urls failed (stored in failed_urls). Download them yourself and create the epub manually.'
            }
    done

    test -z "$hasFailed" && { ec "Converting to epub ..."
                              ecdbg files to send to h2ed *
                              html2epub "$1" "$author" * #.html
                              mv *.epub ../ && cd '../' && \rm -r "./$u"
                              ec "Book '$1' by '$author' has been converted successfully."
    } || { ecerr "$hasFailed" && (exit 1) }
}
w2e-raw() {
    web2epub "$1" "${@:2}" && 2m2k "$1.epub"
}
w2e-o() {
    wr_force=y w2e-raw "$1" "${(@f)$(outlinify "${@:2}")}"
}
emn() {
    emc -e '(helm-man-woman "")' # can't input to helm using its arg. why?
    #"(woman \"$*\")"
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
jup() {
    # ecdbg ./**(D)
    rex "mv _ ${1:-./}" ./**/*(.D)
    #possibly silence it
}
jimg() {
    test "$1" = "-h" && {
        ec 'googleimagesdownload --keywords "Polar bears, baloons, Beaches" --limit 20
googleimagesdownload -k "Polar bears, baloons, Beaches" -l 20

--format svg
-co red : To use color filters for the images
-u <google images page URL> : To download images from the google images link
--size medium --type animated
--usage_rights labeled-for-reuse
--color_type black-and-white
--aspect_ratio panoramic
-si <image url> : To download images which are similar to the image in the image URL that you provided (Reverse Image search).
--specific_site example.com
'
        return
    }
    googleimagesdownload "$@" && jup
}
clean-dups() {
    sort -u "$1" | sponge "$1"
}
jclosh() clojure -Sdeps '{:deps {closh {:git/url "https://github.com/dundalek/closh.git" :tag "v0.4.0" :sha "17e62d5bceaa0cb65476e00d10a239a1017ec5b8"}}}' -m closh.zero.frontend.rebel
@s() {
    googler -j -w 'spotify.com' --url-handler echo "${(@f)$(google-quote "$@")}"
}
google-quote() {
    map '"$1"' "$@"
}
w2e-lw-raw() {
    we_author=LessWrong web2epub "$1" "${(@f)$(re lw2gw "${@:2}")}" && 2m2k "$1.epub"
}
lw2gw() rgx "$1" 'lesswrong\.com' greaterwrong.com
html2epub-pandoc-simple() {
    ecdbg "h2e-ps called with $@"
    pandoc --toc -s "${@:3}" --epub-metadata <(ec "<dc:title>$1</dc:title> <dc:creator> $2 </dc:creator>") -o "$1.epub"
}
aa2e() {
    ecerr DEPRECATED: Use w2e-curl.
    aget "aa -Z ${@:2:q}
html2epub-pandoc-simple $1:q ${${author:-aa2e}:q} *
mv $1:q.epub ../"
    2m2k "$1".epub
}
erase-ansi() {
    gsed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[mGK]//g"
}
ea() {
    eval "$(gquote "$@")" | erase-ansi
}
tldr() nig ea command tldr "$@"
pre-files() {
    doc 'stdin should be null-separated list of files that need replacement; $1 the string to replace, $2 the replacement.'
    comment '-i backs up original input files with the supplied extension (leave empty for no backup; needed for in-place replacement.)(do not put whitespace between -i and its arg.)'
    comment '-r, --no-run-if-empty
              If  the  standard input does not contain any nonblanks,
              do not run the command.  Normally, the command  is  run
              once  even  if there is no input.  This option is a GNU
              extension.'

    AGR_FROM="$1" AGR_TO="$2" gxargs -r0 perl -pi"$pf_i" -e 's/\Q$ENV{AGR_FROM}\E/$ENV{AGR_TO}/g'
}
pdf-count() {
    pdfinfo "$1" | grep Pages|awk '{print $2}'
}
k2pdf-split() {
    doc usage: pdf k2pdf-options
    local s=0 pc="$(pdf-count "$1")" p="${k2_pages:-100}"
    local e i=0
    e=$[s+p]
    until test $s -gt $pc
    do
        k2pdf "$@" -p "$s-$e" -o "%fp$i %b _pages ${s} to ${e}.pdf"
        s=$e
        e=$[s+p]
        i=$[i+1]
    done
}
jaaks() {
    jee
    aa "$@"
    local ag_f=(*)
    aget 'jks ; jup ../'
}
jks() {
    ecdbg entering jks with jufile "$jufile"
    jej
    ecdbg trying to set orig
    local orig=(*)
    ecdbg orig: "$orig"
    k2pdf-split "$orig"
    ecdbg "trying to rm $orig"
    \rm "$orig"
    re 2ko *
}
ensure-ju() {
    test -e "$jufile" || { ecerr "jufile doesn't exist"
                           return 1 }
    local files=(*)
    test -e "$files" || { ecerr "jufile error: $jufile"
                          return 1 }
}
ensure-empty() {
    (silence eval 'comment *(D)') && {
        ecerr Directory "$(pwd)" not empty
        return 1
    } || return 0
}
get-dl-link() {
    ec "${dl_base_url:-http://lilf.ir:8080/}$(realpath --relative-to ~/Downloads "$1")"
}
jdl-helper() {
    mkdir -p ~/Downloads/tmp/
    mv "$1" ~/Downloads/tmp/
    get-dl-link ~/Downloads/tmp/"${1:t}"
}
jdl() {
    jej
    re jdl-helper *(D)
}
emj-old () emoj --copy "$*"
emj() emoji-finder "$@"
2m2k2h() { 2m2k "$@" && { trs "$1"
                          trs "${1:r}.mobi" } }
warm-zlua() {
    fc -ln 0|grep -o "^cd [~/].*"|sed -e "s|cd ||" -e "s|~|$HOME|" -e 's|\\ | |' -e "s|/$||"|sort|uniq|while read -r d; do test -d "$d" && echo "$d|1|0"; done >> ~/.zlua
}
ask() {
    doc 'This is a general-purpose function to ask Yes/No questions in Bash, either with or without a default answer. It keeps repeating the question until it gets a valid answer.'

    # https://gist.github.com/davejamesmiller/1965569
    local prompt default reply

    if [ "${2:-}" = "Y" ]; then
        prompt="Y/n"
        default=Y
    elif [ "${2:-}" = "N" ]; then
        prompt="y/N"
        default=N
    else
        prompt="y/n"
        default=
    fi

    while true; do

        # Ask the question (not using "read -p" as it uses stderr not stdout)
        echo -n "$1 [$prompt] "

        # Read the answer (use /dev/tty in case stdin is redirected from somewhere else)
        read reply </dev/tty

        # Default?
        if [ -z "$reply" ]; then
            reply=$default
        fi

        # Check if the reply is valid
        case "$reply" in
            Y*|y*) return 0 ;;
            N*|n*) return 1 ;;
        esac

    done
}
function pk() {
    pgrep -i "$@"
    pkill -9 -i "$@"
}
