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
    local case_sensitive="${duplicates_clean_case_sensitive:-y}"
    ensure-array duplicates_clean_exceptions
    local exceptions=("${duplicates_clean_exceptions[@]}")

    {
        cat-paste-if-tty |
            prefixer --tac --skip-empty |
            perl -e '
                $case = shift @ARGV eq "y";
                $exceptions = join("|", map { quotemeta } @ARGV);
                while (<STDIN>) {
                    chomp;
                    next unless /\S/;
                    if ($exceptions && /^(?:$exceptions)$/) {
                        print "$_\n";
                    } else {
                        $k = $case ? $_ : lc($_);
                        print "$_\n" unless $seen{$k}++;
                    }
                }
            ' "${case_sensitive}" "${exceptions[@]}" |
            prefixer --tac --skip-empty @RET
        ec #: add an end separator
    } | cat-copy-if-tty
}
aliasfn duplicates-rm duplicates-clean
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

function trimsed {
     in-or-args "$@" |
        gsed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//' |
        cat-copy-if-tty
}

function trimpy {
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
function char2ascii {
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

function ascii2char {
    [ "$1" -lt 256 ] || return 1
    printf "\\$(printf '%03o' "$1")" | cat-copy-if-tty
}
##
function char2unicode {
    : "@alt [help:describe-char]"

    : "UTF-8"
    perl -CA -le 'print ord shift' "$1" | cat-copy-if-tty
}

function unicode2char {
    unicode2char-hex "$(printf %08x "${1}")"

    ## @tests
    # `unicode2char 128153`
    #   blue heart
    ##
}

function unicode2char-hex {
    # works with emojis
    printf "\U${1}" | cat-copy-if-tty
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
    in-or-args "$@" |
        python -c 'from unidecode import unidecode ; import sys ; print(unidecode(sys.stdin.read()))' |
        cat-copy-if-tty

    # You can also specify an errors argument to unidecode() that determines what Unidecode does with characters that are not present in its transliteration tables. The default is 'ignore' meaning that Unidecode will ignore those characters (replace them with an empty string). 'strict' will raise a UnidecodeError. The exception object will contain an index attribute that can be used to find the offending character. 'replace' will replace them with '?' (or another string, specified in the replace_str argument). 'preserve' will keep the original, non-ASCII character in the string. Note that if 'preserve' is used the string returned by unidecode() will not be ASCII-encodable!
}

# aliasfn utf8-to-ascii utf8-to-ascii-iconv
aliasfn utf8-to-ascii utf8-to-ascii-pyunidecode
aliasfn str-normalize utf8-to-ascii

function str-normalize2 {
    in-or-args "$@" |
        unicode_normalizer_hf.py "$@" |
        cat-copy-if-tty
}

function newline-normalize {
    cat-paste-if-tty |
        perl -CS -0777 -pe 'BEGIN { use utf8; use open qw/:std :utf8/; } ; s/\R/\n/g' |
        cat-copy-if-tty
}
##
function str-bad-characters-rm {
    in-or-args "$@" |
        perl -CS -ple 's/\h+\\xb.\h*/ /g' |
        cat-copy-if-tty
}
##
function bullets-to-dash {
    in-or-args "$@" |
    sd '•' '-' |
        cat-copy-if-tty
}
##
function tab2space {
    integer space_count="${1:-4}"

    cat-paste-if-tty |
        revaldbg perl -CS -lpe 's/^\t+/(" " x '${space_count}') x length($&)/e' |
        cat-copy-if-tty
}

function space2tab {
    integer space_count="${1:-4}"

    cat-paste-if-tty |
        revaldbg perl -CS -lpe 's<^( {'${space_count}'})+><("\t") x int(length($&)/'${space_count}')>e' |
        cat-copy-if-tty
}
##
function whitespace-shared-rm {
    cat-paste-if-tty |
        whitespace_shared_rm.pl "$@" |
        cat-copy-if-tty
}
aliasfn strip-prefixed-whitespace whitespace-shared-rm

function trailing-whitespace-rm {
    cat-paste-if-tty |
    perl -lpe 's/\s+$//' |
        cat-copy-if-tty
}
function end-trailing-whitespace-rm {
    #: Removes whitespace exactly at the end of the file (e.g., newlines at the end)
    #: \Z Match only at end of string, or before newline at the end
    #: \z Match only at end of string
    ##
    cat-paste-if-tty |
    perl -0777 -pe  's/\s+\z//' |
        cat-copy-if-tty
}
##
function floatsort {
    cat-paste-if-tty |
        floatsort.rs "$@" |
        cat-copy-if-tty
}
aliasfn fsort floatsort

function sort-last-float-rust {
    #: This is much faster than `sort-last-float-perl'.
    ##
    floatsort "$@"
}
aliasfn sort-last-float sort-last-float-rust

function sort-last-float-perl {
    #: Sorts such that the last line is the highest number.
    #: You can use `tac` to reverse the order.
    ##

    perl -e '
    # $float_pat = qr/([-+]?\d+(?:\.\d*)?)\D*$/;
    $float_pat = qr/([-+]?\d+(?:\.\d*)?(?:[eE][-+]?\d+)?)\D*$/;

    print sort {
        ($a =~ $float_pat)[0] <=> ($b =~ $float_pat)[0]
    } <>
'
    #: - `print sort { ... } <>`: Reads input lines (from a file specified as an argument or from standard input), sorts them based on the block of code provided, and prints the sorted lines.
    #: - `($a =~ /.../)[0]`: For each line in `$a`, this regular expression matches the last number appearing just before a semicolon. The number can be an integer or a floating-point number. The `[0]` at the end extracts the first match from the list of matches (which, in this case, is the only match).
    #: - `<=>`: The spaceship operator is used for numeric comparison between the extracted numbers from `$a` and `$b` (where `$a` and `$b` are the default variables used by Perl's `sort` function to hold the items being compared).
    #: - `<>`: This is the diamond operator, used here to read input from either standard input or from files specified as command-line arguments.
}

##
function sort-by-regex-perl {
    #: @duplicateCode/22b86ed758257c1f1036c9b3cdf93542
    #:
    #: @alt [[NIGHTDIR:rust/floatsort.rs][floatsort]]
    #:
    #: @example
    #: You can sort the lines by the 'n' number first, and if the numbers are the same, by the fruit name:
    #: echo -e 'apple 1 n:10\nbanana 2 n:20\ncherry 3 n:10\ndate 4 n:30' |
    #:   sort-by-regex-perl -a 'n:(\d+)' -d '(\w+)'
    ##
    if [[ $# -lt 1 ]]; then
        echo "Usage: sort-by-regex-perl [-a|--ascending] [-d|--descending] <pattern1> [<pattern2> ...] [--]"
        return 1
    fi

    perl -e '
    use strict;
    use warnings;
    use v5.34.0;
    use Getopt::Long;

    my @patterns;
    my @orders;
    my $default_order = "ascending";
    # my $default_order = "descending";
    my $current_order = $default_order;

    GetOptions(
        "a|ascending" => sub { $current_order = "ascending"; },
        "d|descending" => sub { $current_order = "descending"; },
        "<>" => sub {
            push @patterns, { pattern => $_[0], order => $current_order };
            $current_order = $default_order;
        }
    );

    if (@patterns == 0) {
        die "Usage: sort-by-regex-perl [-a|--ascending] [-d|--descending] <pattern1> [<pattern2> ...] [--]\n";
    }

    my @lines = <STDIN>;

    print sort {
        my $result = 0;
        for my $entry (@patterns) {
            my $pattern = $entry->{pattern};
            my $order = $entry->{order};
            my ($a_match) = $a =~ /$pattern/;
            my ($b_match) = $b =~ /$pattern/;
            $a_match //= "";
            $b_match //= "";

            # Use numeric comparison if both matches are numbers, otherwise use string comparison
            if ($a_match =~ /^-?\d+(?:\.\d+)?$/ && $b_match =~ /^-?\d+(?:\.\d+)?$/) {
                $result = $a_match <=> $b_match;
            } else {
                $result = $a_match cmp $b_match;
            }

            # print STDERR "Comparing $a_match (from pattern $pattern) and $b_match: $result\n";

            $result = -$result if $order eq "descending";
            last if $result != 0;
        }
        $result;
    } @lines;
    ' -- "$@"
}
##
function lower {
    local input
    input="${$(in_or_args_newline_p=n in-or-args "$@" ; print -n .)[1,-2]}" @RET

    ecn "${input:l}" |
        cat-copy-if-tty
}
##
