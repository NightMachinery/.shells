### Module Text Manipulation
### This module specializes in functions that do not touch the disk.
###
function prefix-if-ne() {
    : "prefix if not empty: <prefix> <output> [<optional-checks> ...]"
    local prefix="$1" out="${2}" ; shift 1 || return $?
    (( $#@ >= 1 )) || return 0 # Empty (unset) out
    local ne=y
    local i
    for i in "$@" ; do
       [[ "$i" =~ '\S' ]] || ne='' # out and all optional-checks should be non-empty
    done
    if test -n "$ne" ; then
        ecn "${prefix}$out"
    else
        ecn "$out"
    fi
}
function dedent() {
    sd --flags m '^\s*' ''
}
function trim() {
    : "Doesn't trim whitepsace in each line separately, use trimsed for that"

    local out="$(in-or-args "$@")"
    [[ "$out" =~ '(?im)^\s*((?:.|\n)*\S)\s*$' ]] && out="$match[1]" || out=''
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
##
function prefixer-rm() {
    local input
    input="$(cat)" || return 3
    ecn $input | prefixer rm -- "${(@f)$(ecn $input | reval "$@")}"
}
aliasfn prm prefixer-rm
##
function text2num() {
    # reverse op: https://github.com/kslazarev/numbers_and_words
    # Roman numerals https://github.com/allo-media/text2num/issues/43
    ##
    # prefixer will not replace 'IV\n' so we should not put '\n' at the end
    ecn "$(in-or-args "$@")" | prefixer replace -i ' ' -o ' ' II 2 III 3 IV 4 V 5 VI 6 VII 7 VIII 8 IX 9 | text2num.py
}
