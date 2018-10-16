it2prof() { echo -e "\033]50;SetProfile=$1\a" } # Change iterm2 profile. Usage it2prof ProfileName (case sensitive)
file-to-clipboard() {
    osascript \
        -e 'on run args' \
        -e 'set the clipboard to POSIX file (first item of args)' \
        -e end \
        "$@"
}
function dl-stream() {
    aria2c "$1" &
    #TODO
    #  "${2:i-iina}" ""
}
function cpsd() {
    local i;
    local counter=1;
    local outs=();
    for i in "$@"; do
        local B=$(basename "$i"); local D=$(dirname "$i");
        local out="$D/${B%.*}.png"
        convert "$i""[0]" "$out"
        outs[$counter]=$out
        counter=$(($counter + 1))
    done
    pbadd "${(@)outs}"
}
function set-fk-icon-size() {
    /usr/libexec/PlistBuddy -c "set FK_DefaultIconViewSettings:iconSize ${1:-128}" ~/Library/Preferences/com.apple.finder.plist # This is for Finderkit, i.e., dialogs.
                            }
function set-finder-icon-size() {
    /usr/libexec/PlistBuddy -c "set StandardViewSettings:IconViewSettings:iconSize ${1:-128}" ~/Library/Preferences/com.apple.finder.plist # This is for Finder itself.
}
function loop() { while true; do eval "${@:2}"; sleep ${1:-1}; done }
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
function pbadd() {
    osascript ~/'scripts/applescript/path-copy.applescript' "$@" > /dev/null
}
function sdc() {
    it2prof Default
    sdcv --color "$*" | less
    it2prof 'Hotkey Window'
}
function sp() { ispell<<<"$*" } 
