export nightNotes="$cellar/notes/"
export nightJournal="$nightNotes/journal"
###
alias imd='img2md-imgur'
alias unt='url2note'
alias nts='ntsearch'
###
function vnt() {
    ntsearch "$@" || return 1
    reval "${veditor}" -p "$outFiles[@]"
}
function ntsearch() {
    out=''
    out=( "${(@0)$(ntsearch_ "$@")}" ) || return 1
    out=( "${(@)out[1,-2]}" ) # remove empty last element that \0 causes
    outFiles=()
    local i
    for i in "$out[@]"
    do
           outFiles+="$nightNotes$(<<<"$i" head -n 1)"
    done
    ec "$out[@]"
}
function ntsearch_() {
    local pattern="$*"
    local file files=( "${(@f)$(fd -e md -e txt -e org ${pattern:-.} $nightNotes)}" )
    local first='y'
    for file in "$files[@]"
    do
        test -n "$first" || print -n $'\0'
        first=''
        color 30 90 255 "$(realpath --relative-to $nightNotes $file)"$'\n'
        cat $file
    done  | fz --preview-window right --preview "$FZF_SIMPLE_PREVIEW" --ansi --read0 --print0
    # right:hidden to hide preview
    # | gawk 'BEGIN { RS = "\0" ; ORS = RS  } ;  NF'
}
function jrlt() {
	local today="$(date +"%Y.%b.%d") $(datej|tr / -)"
	local dest="$nightJournal/$today.md"
	test -e "$dest" || {
	ec "# $today"$'\n\n'"* " >> $dest
	}
	! isI || $EDITOR[@] $dest
}
function img2md() {
    mdoc "$0 <picture-file> [<description>]
Outputs the image in markdown format, hardcoded in base64. Large images (~0.3 MB) will probably crash the system though." MAGIC

    jglob

    local file="$1" desc="$2" 
    local compressed="$(gmktemp --suffix .jpg)" # ".${file:e}"
    convert $file -define jpeg:extent=150kb $compressed # 200kb didn't work
    file=$compressed
    doc use base64 from brew to ensure consistency
    ## python base64 (might work in eva):
    # encoded_string= base64.b64encode(img_file.read())
    # print(encoded_string.decode('utf-8'))
    
    # somehow breaks in eva_aget ...
    print -r -- "![$desc](data:$(file -b --mime-type $file);base64,$(base64 "$file" | tr -d '\r\n'))"
}
function img2md-imgur() {
    mdoc "$0 <picture-file> [<description>]
Outputs the image in markdown format, hosted on imgur." MAGIC

    jglob
    local file="$1" desc="$2" 

    print -r -- "![$desc]($(imgurNoD=y imgur.bash $file))"
}
function url2note() {
    local url="$1"
    local title="$(urlmeta $url title)"
    local desc="$(urlmeta $url description)"
    local img="$(urlmeta $url image)"

    ec "* [${title:-$url}]($url)"
    test -z "$desc" || ec "  * $desc"
    test -z "$title" || ec "  * $url"
    test -z "$img" || ec "![]($img)"

}
