##
alias zre='regexp-replace' #Change to function and add bash fallback

function strip {
    #: @nameConflict with the strip command
    local x="$1"
    zre x "^$2" ''
    zre x "$2"'$' ''
    ec "$x"
    # local STRING="${1##"$2"}"
    # ec "${STRING%%"$2"}"
}

function rgx {
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
    rgbase --no-filename --no-line-number --text --smart-case --only-matching --replace "$rep" "$@"
}
##
function text-wrap {
    local w="${1:-${text_wrap_columns:-${COLUMNS:-90}}}"

    if (( ${+commands[ansifold]} )) ; then
        ansifold --boundary=word --width="$w" --padchar=' ' "${@[2,-1]}"
        # --paragraph adds extra newlines for paragraphs
        # --padding adds extra whitespace to the end of lines and makes their width uniform
    else
        ecgray "$0: ansifold not available, falling back to ggrep."
        command ggrep -Eo ".{1,$w}" # unicode-safe (RTL safe)
    fi |
        cat-copy-if-tty
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
    cat-paste-if-tty | rg --fixed-strings --count ''
    # wc will not count the last line if it does not end with '\n':
    # https://stackoverflow.com/questions/28038633/wc-l-is-not-counting-last-of-the-file-if-it-does-not-have-end-of-line-character
}
##
function hex2ascii {
    xxd -r -p
    # -r tells it to convert hex to ascii as opposed to its normal mode of doing the opposite
    # -p tells it to use a plain format.
    ## tests:
    # `ec '787465726d2d6b69747479' | hex2ascii`
    ##
}
##
function case-title {
    local text
    text="$(in-or-args "$@")" @RET

    command titlecase "$text" # from python
}
##
function prefix-rm {
    local prefixes=("$@")
    if (( ${#prefixes} == 0 )) ; then
        cat
        return $?
    fi

    # @todo @perf add multiple prefix removal to prefixer
    local p cmd=''
    for p in "$prefixes[@]" ; do
        cmd+="rmprefix $(gquote $p) |"

        # I want to write a function that gets a number n for its argument, and then runs a pipe n times. For example, for n=3, it should run:
        #  cat | some-program | some-program | some-program
        #  I can do this using 'eval', but I am loath to do that. Any eval-less (or at least eval-light) solutions?
        #  (Obviously no temporary files. I want the whole pipe to run in parallel.)
    done

    evaldbg "${cmd[1,-2]}"
}
##
function strip-blank-lines-start-end {
    #: reads stdin
    ##
    cat-paste-if-tty |
        strip_blank_lines_start_end.pl |
        cat-copy-if-tty
}

function strip-duplicate-subsequent-blank-whitespace-lines {
    #: reads stdin
    ##
    cat-paste-if-tty |
        perl -ne 'print if /\S/ || !$seen++; $seen=0 if /\S/' |
        cat-copy-if-tty
}
##
