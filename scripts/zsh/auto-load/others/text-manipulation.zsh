### Module Text Manipulation
### This module specializes in functions that do not touch the disk.
###
dedent() {
    sd --flags m '^\s*' ''
}
function trim() {
    local out="$*"
    [[ "$out" =~ '^\s*(.*\S)\s*$' ]] && out="$match[1]" || out=''
    print -nr -- "$out"
}
trimsed() {
    gsed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'
}
trimpy() {
    python3 -c 'import sys
for line in sys.stdin: print(line.strip())'
}
function removeTrailingSlashes() {
    case $1 in
        *[!/]*) ec "$1"|sed 's:/*$::' ;; #x=${x%"${x##*[!/]}"};;
        [/]*) ec "/";;
    esac
}
function str-normalize() {
    iconv -f utf-8 -t ascii//translit
}
