alias zre='regexp-replace' #Change to function and add bash fallback
function strip() {
    # TODO name conflicts with the strip command
    local x="$1"
    zre x "^$2" ''
    zre x "$2"'$' ''
    ec "$x"
    # local STRING="${1##"$2"}"
    # ec "${STRING%%"$2"}"
}
rgx() {
    local a
    (( $# == 2 )) && a="$(</dev/stdin)" || { a="$1" ; shift 1 }
    zre a "$1" "$2"
    ec "$a"
}
function sdlit() {
    # alt: `sd -s` ; perf is almost the same, but sd is probably a (very small) bit faster.
    local search="$1" replace="$2"
    
    rmprefix '' "$search" "$replace"
}
##
function rget() {
    local rep="${rget_replace:-${rget_r:-\$1}}"
    command rg --no-filename --no-line-number --text --smart-case --only-matching --replace "$rep" "$@"
}
##
function text-wrap() {
    local w="${1:-${COLUMNS:-90}}"

    if (( ${+commands[ansifold]} )) ; then
        ansifold --boundary=word --width="$w" --padchar=' ' "${@[2,-1]}"
        # --paragraph adds extra newlines for paragraphs
        # --padding adds extra whitespace to the end of lines and makes their width uniform
    else
        ecgray "$0: ansifold not available, falling back to ggrep."
        command ggrep -Eo ".{1,$w}" # unicode-safe (RTL safe)
    fi
}
alias foldm='text-wrap'
##
function erase-nonascii {
    perl -ple 's/[^[:ascii:]]//g'
}
##
function isSpace {
    local inargs
    in-or-args2 "$@"

    ecn "$inargs[*]" | perl -0777 -ne '/\A\s*\Z/ && exit 0 || exit 1'
}
##
function count-lines {
    rg --fixed-strings --count ''
    # wc will not count the last line if it does not end with '\n':
    # https://stackoverflow.com/questions/28038633/wc-l-is-not-counting-last-of-the-file-if-it-does-not-have-end-of-line-character
}
##
