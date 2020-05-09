export nightNotes="$cellar/notes/"
export nightJournal="$nightNotes/journal"
##
function vn() {
    rn "$@" || return 1
    reval "${veditor}" -p "$outFiles[@]"
}
function rn() {
    out=''
    out=( "${(@0)$(rn_ "$@")}" ) || return 1
    out=( "${(@)out[1,-2]}" ) # remove empty last element that \0 causes
    outFiles=()
    local i
    for i in "$out[@]"
    do
           outFiles+="$nightNotes$(<<<"$i" head -n 1)"
    done
    ec "$out[@]"
}
function rn_() {
    local pattern="$*"
    local file files=( "${(@f)$(fd -e md ${pattern:-.} $nightNotes)}" )
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
    doc needs base64 from brew
    # somehow breaks in eva_aget ...
    print -nr -- "![$desc](data:$(file -b --mime-type $file);base64,$(base64 --encode "$file"))"
}
