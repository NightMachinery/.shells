### Module Text Manipulation
### This module specializes in functions that do not touch the disk.
###
function duplicates-clean-sort-file-inplace {
    local f="$1"
    test -e "$f" @TRET

    gsort -u "$f" | sponge "$f"
}

function duplicates-clean {
    #: @seeAlso [agfi:mpv-bookmark-cleanup]
    ##
    prefixer --tac --skip-empty \
        | gawk 'NF && !seen[$0]++' \
        | prefixer --tac --skip-empty @RET
    ec #: add an end separator
}
aliasfn duplicates-clean-file-inplace inplace_io_m=last_stdin_stdout inplace-io duplicates-clean
function duplicates-clean-nul {
    prefixer -i '\x00' -o '\x00' --tac --skip-empty \
        | gawk 'BEGIN { RS="\0";  ORS="\0" } NF && !seen[$0]++' \
        | prefixer -i '\x00' -o '\x00' --tac --skip-empty
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

function trim {
    : "Doesn't trim whitepsace in each line separately, use trimsed for that"

    local out="$(in-or-args "$@")"
    [[ "$out" =~ '(?im)^\s*((?:.|\n)*\S)\s*$' ]] && out="$match[1]" || out=''
    print -nr -- "$out"
}

function trimsed() {
    gsed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'
}

function trimpy() {
    python3 -c 'import sys
for line in sys.stdin: print(line.strip())'
}

function trim-extension {
    perl -ple 's/\.[^.]+$//g'
}

function removeTrailingSlashes() {
    case $1 in
        *[!/]*) ec "$1"|sed 's:/*$::' ;; #x=${x%"${x##*[!/]}"};;
        [/]*) ec "/";;
    esac
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
function double-newlines() {
    # awk 'ORS="\n\n"'
    ##
    cat-paste-if-tty | prefixer -o $'\n\n' | cat-copy-if-tty
    ##
}

function p-double-newlines() {
    pbpaste | double-newlines | cat-copy-if-tty
}
alias pdn='p-double-newlines'
##
function newline2space {
    cat-paste-if-tty |
        perl -CS -pe 'BEGIN { use utf8; use open qw/:std :utf8/; } ;
        s/^\h+//g if $. == 1 ;
 s/\x{2}//g ; s/\R\s*/ /g ; s/\h+/ /g ; s/(*plb:\w)-\h//g' |
        cat-copy-if-tty

    #: =\x{2}= is the ASCII character two, which in emacs shows as =^B= and can be inserted using [kbd:C-q C-b].
    #: =(*plb:\w)-\h= changes, e.g., =com- ponent= to =component=
}

function p-newline2space {
    pbpaste | newline2space | cat-copy-if-tty
}
alias pns='p-newline2space'
##
function char2ascii() {
    ##
    LC_CTYPE=C printf '%d' "'${1[1]}" | cat-copy-if-tty
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
    printf "\\$(printf '%03o' "$1")" | cat-copy-if-tty
}
##
function char2unicode {
    : "@alt [help:describe-char]"

    : "UTF-8"
    perl -CA -le 'print ord shift' "$1" | cat-copy-if-tty
}

function unicode2char() {
    unicode2char-hex "$(printf %08x "${1}")"

    ## @tests
    # `unicode2char 128153`
    #   blue heart
    ##
}

function unicode2char-hex() {
    # works with emojis
    printf "\U${1}\n" | cat-copy-if-tty
}
##
function str2unicode-hex {
    # https://github.com/janlelis/unicopy
    # `gemi unicopy`
    ##
    local input
    input="$(in-or-args "$@")"

    revaldbg unicopy --string "$input" --print --uplus | cat-copy-if-tty
}

function unicode2str-hex {
    local input
    input="$(in-or-args "$@")"

    revaldbg unicopy "$input" --print | cat-copy-if-tty

    ## @tests
    # `unicode2str-hex 'U+01F5A4' | tee /dev/tty | str2unicode-hex`
    #   should print a black heart emoji
    ##
}
##
function char2octal {
    perl -e '(printf "%o\n", ord $_) foreach @ARGV' "$@" | cat-copy-if-tty
}

function char2hex {
    perl -e '(printf "0x%X\n", ord $_) foreach @ARGV' "$@" | cat-copy-if-tty
}
##
function str-repeat {
    local str="${@[2,-1]}" sep="${str_repeat_sep}"
    integer n="$1"

    if (( n < 1 )) ; then
        return 0
    fi

    {
        local i
        for i in {1..$n} ; do
            ecn "${str}"
            if (( i < $n )) ; then
                ecn "${sep}"
            fi
        done
    } | cat-copy-if-tty
}
##
function utf8-to-ascii-iconv {
    iconv -f utf-8 -t ascii//TRANSLIT -c "$@" | cat-copy-if-tty
    # -c ignore conversion errors silently
}


function utf8-to-ascii-uni2ascii {
    # http://billposer.org/Software/uni2ascii.html
    #
    # `brew install uni2ascii`
    ##
    serrdbg uni2ascii -e -x "$@" | cat-copy-if-tty
    # -e Convert characters to their approximate ASCII equivalents
    # -x Expand certain characters to  multicharacter  sequences.
}

function utf8-to-ascii-perl {
    perl -pe 'BEGIN { use utf8; use Text::Unidecode } ; unidecode($_)'
}

function utf8-to-ascii-translit {
    # cpanm 'Lingua::Translit'
    ##
    # @warn this still outputs "bad" characters
    ##
    translit -t "ISO 843" "$@" | cat-copy-if-tty
}

function utf8-to-ascii-pyunidecode {
    # https://pypi.org/project/Unidecode/
    #
    # `pip-install Unidecode`
    ##
    python -c 'from unidecode import unidecode ; import sys ; print(unidecode(sys.stdin.read()))' | cat-copy-if-tty

    # You can also specify an errors argument to unidecode() that determines what Unidecode does with characters that are not present in its transliteration tables. The default is 'ignore' meaning that Unidecode will ignore those characters (replace them with an empty string). 'strict' will raise a UnidecodeError. The exception object will contain an index attribute that can be used to find the offending character. 'replace' will replace them with '?' (or another string, specified in the replace_str argument). 'preserve' will keep the original, non-ASCII character in the string. Note that if 'preserve' is used the string returned by unidecode() will not be ASCII-encodable!
}

# aliasfn utf8-to-ascii utf8-to-ascii-iconv
aliasfn utf8-to-ascii utf8-to-ascii-pyunidecode
aliasfn str-normalize utf8-to-ascii

function str-normalize2 {
    unicode_normalizer_hf.py "$@" | cat-copy-if-tty
}

function newline-normalize {
    perl -0777 -pe 's/\R/\n/g'
}
##
function str-bad-characters-rm {
    in-or-args "$@" |
        perl -CS -ple 's/\h+\\xb.\h*/ /g' |
        cat-copy-if-tty
}
##
