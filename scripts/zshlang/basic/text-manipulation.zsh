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
    command rg --text --smart-case --only-matching --replace '$1' "$@"
}
##
function text-wrap() {
    local w="${1:-90}"

    command ggrep -Eo ".{1,$w}" # unicode-safe
}
alias foldm='text-wrap'
##
