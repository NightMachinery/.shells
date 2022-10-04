function passgen() {
    : "@alt passgen.go"

    reval-copy openssl rand -base64 "${1:-16}"
}

function passgen-numerical {
    local len="${1:-16}"

    local hex
    hex="$(openssl rand -hex "$len")" @TRET

    local dec
    dec="$(revaldbg hex2decimal "${hex}")" @TRET

    ec "${dec[1,$len]}" # deciaml is longer than hex
}

function passgen-words {
    local len="${1:-10}"

    local words=() n w
    while (( ${#words} < len )) ; do
        n="$(passgen-numerical 6)" @TRET
        w="$(num2words "$n")" @TRET
        if ! isSpace "$w" ; then
            words+="$w"
        fi
    done

    ec "${(j.-.)words[@]}" | cat-copy-if-tty
}
##
function with-dice-i {
    fnswap passgen-numerical passgen-numerical-dice-i "$@"

    bell-entropy-gained
}

aliasfn passgen-words-dice-i with-dice-i passgen-words

function passgen-numerical-dice-i {
    local len="${1:-6}" base="${2:-6}"

    local needed_dice
    needed_dice="$(@opts from 10 to $base @ num-base-convert-py "$(str-repeat "$len" 9)")" @TRET
    needed_dice="${#needed_dice}"

    ecbold "$0: needed_dice=${needed_dice}"

    local dice=()
    local i
    while (( ${#dice} < $needed_dice )) ; do # @noflycheck
        local die
        die="$(vared-gateway "enter die value: ")" || {
            ecerr "$0: aborted"
        }
        if (( die > 0 && die <= base )) ; then
            dice+="$die"
        else
            ecerr "$0: illegal die number $(gquote-sq "$die"); Try again."
        fi
    done

    local n
    n="$(dice2decimal "${(j..)dice}" "$base")" @TRET

    printf "%0*d" "$len" "${n[1,$len]}"
}
##
function num2words {
    local n=("$@") wordlist="${num2words_l:-$WORDLIST0}"
    assert-args n @RET
    assert test -e "$wordlist" @RET

    local wordlist_len
    wordlist_len="$(cat "$wordlist" | count-lines)" @TRET
    local i n_safe=()
    for i in $n[@] ; do
        n_safe+=$(( (i % wordlist_len) + 1 ))
        # @warn this trick will cause the probability distribution of the output to be somewhat non-uniform; E.g., think we have a list of ['a', 'b'], and then we generate a uniform number from 0 to 2; Then 'a' will be twice as probable to be selected: p(a) = 2/3 ; p(b) = 1/3
        # if we don't do this trick, then unsafe inputs will be rejected, which means that we will waste a lot of random bits, which is not tolerable when using manual dice.
    done

    cat "$wordlist" | revaldbg prefixer --included-only --process-include="${(j.,.)n_safe}"
}
##
function pass-check() {
    # * pass_check_additional: user data to be added to the dictionaries that are tested against (name, birthdate, etc)
    # * Check out `crack_times_display` in the output
    # * Gives a score to the password, from 0 (terrible) to 4 (great)
    ec "$*" | zxcvbn --user-input "$pass_check_additional" | json-beautify
}
##
