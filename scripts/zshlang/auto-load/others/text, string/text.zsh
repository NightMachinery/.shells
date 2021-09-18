### Module Text Manipulation
### This module specializes in functions that do not touch the disk.
###
function whitespace-is() {
    in-or-args2 "$@"
    if ecn "$inargs[*]" | silent command rg '\S' ; then
        return 1
    else
        return 0
    fi
}
##
clean-dups() {
    sort -u "$1" | sponge "$1"
}
##
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
##
function numfmt-bytes() {
    numfmt --to=iec-i --suffix=B "$@"
}
##
function double-newlines() {
    # awk 'ORS="\n\n"'
    ##
    prefixer -o $'\n\n'
    ##
}

function p-double-newlines() {
    pbpaste | double-newlines | cat-copy
}
alias pdn='p-double-newlines'
##
function newline2space {
    sd '\n\s*' ' '
}

function p-newline2space {
    pbpaste | newline2space | cat-copy
}
alias pns='p-newline2space'
##
function char2ascii() {
    ##
    LC_CTYPE=C printf '%d' "'${1[1]}"
    ##
    # ecn "${1[1]}" | command od -An -tuC
    ### Use od (octal dump) to print:
    ### -An  means Address none
    ### -t  select a type
    ###  u  type is unsigned decimal.
    ###  C  of size (one) char.
}
function ascii2char() {
    [ "$1" -lt 256 ] || return 1
    printf "\\$(printf '%03o' "$1")"
}
function char2unicode() {
    # UTF-8
    perl -CA -le 'print ord shift' "$1"
}
function char2octal {
    perl -e '(printf "%o\n", ord $_) foreach @ARGV' "$@"
}
function char2hex {
    perl -e '(printf "0x%X\n", ord $_) foreach @ARGV' "$@"
}
function unicode2char() {
    # works with emojis
    printf "\U$(printf %08x "${1}")\n"
}
##
function str-repeat {
    local str="${@[2,-1]}" sep="${str_repeat_sep}"
    integer n="$1"

    if (( n < 1 )) ; then
        return 0
    fi

    local i
    for i in {1..$n} ; do
        ecn "${str}"
        if (( i < $n )) ; then
            ecn "${sep}"
        fi
    done
}
##
