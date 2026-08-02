##
function passgen-base64 {
    #: @alt =passgen.go=
    ##
    openssl rand -base64 "${1:-16}" |
        cat-copy-if-tty
    #: Base64 is a scheme for converting binary data to printable ASCII characters, namely the upperand lower-case Roman alphabet characters (A–Z, a–z), the numerals (0–9), and the "+" and "/" symbols, with the "=" symbol as a special suffix code.
}

function passgen-alphanumeric {
    passgen-base64 "${@:-32}" |
        tr -dc 'a-zA-Z0-9' |
        cat-copy-if-tty
}

function passgen-hex {
    openssl rand -hex "${1:-16}" |
        cat-copy-if-tty
}
aliasfn passgen passgen-alphanumeric

function passgen-numerical {
    local len="${1:-16}"

    local hex
    hex="$(openssl rand -hex "$len")" @TRET

    local dec
    dec="$(revaldbg hex2decimal "${hex}")" @TRET

    ec "${dec[1,$len]}" | # deciaml is longer than hex
        cat-copy-if-tty
}

function passgen-words {
    #: @alt =passgen_words.py=
    ##
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
function 2fa-code {
    local inargs
    in-or-args3 "$@" @RET

    if ! isdefined-cmd oathtool ; then
        ecerr "$0: missing command: oathtool (install Homebrew 'oath-toolkit' or apt 'oathtool')"
        return 1
    fi

    local input retcode=0
    for input in "${inargs[@]}" ; do
        local period="${twofa_code_period:-30}"
        local secret=""
        local code=""
        local valid_for=""
        local oathtool_opts=()
        local otpauth_params=()

        if test -z "${input}" ; then
            ecerr "Usage: $0 <BASE32_SECRET_OR_OTPAUTH_URL>"
            retcode=1
            continue
        fi

        if [[ "${input}" == otpauth://* ]] ; then
            otpauth_params=("${(@f)$(perl -e '
                use strict;
                use warnings;

                sub url_decode {
                    my ($value) = @_;
                    $value =~ tr/+/ /;
                    $value =~ s/%([0-9A-Fa-f]{2})/chr(hex($1))/eg;
                    return $value;
                }

                my $url = shift // q{};
                my %params;
                if ($url =~ /[?]([^#]*)/) {
                    for my $pair (split /&/, $1) {
                        my ($key, $value) = split /=/, $pair, 2;
                        next unless defined $key;
                        $params{url_decode($key)} = url_decode($value // q{});
                    }
                }

                if (!exists $params{secret} || $params{secret} eq q{}) {
                    print STDERR "secret= not found in otpauth URL", chr(10);
                    exit 1;
                }

                print $params{secret}, chr(10);
                print $params{period}, chr(10) if exists $params{period} && $params{period} ne q{};
            ' "${input}")}") || {
                retcode=$?
                continue
            }
            secret="${otpauth_params[1]}"
            if (( ${#otpauth_params} >= 2 )) ; then
                period="${otpauth_params[2]}"
            fi
        else
            secret="${input}"
        fi

        secret="${secret:u}"
        secret="${secret//[[:space:]-]/}"

        if test -z "${secret}" ; then
            ecerr "$0: empty 2FA secret"
            retcode=1
            continue
        fi

        if [[ ! "${period}" == <1-> ]] ; then
            ecerr "$0: invalid TOTP period: $(gquote-sq "${period}")"
            retcode=1
            continue
        fi

        oathtool_opts=(--totp --base32 --time-step-size="${period}s")
        code="$(command oathtool "${oathtool_opts[@]}" "${secret}")" || {
            retcode=$?
            continue
        }
        ec-copy "${code}"

        valid_for="$(( period - (EPOCHSECONDS % period) ))"
        ecgray "$0: valid for ${valid_for}s"
    done

    return $retcode
}
##
function pass-check() {
    # * pass_check_additional: user data to be added to the dictionaries that are tested against (name, birthdate, etc)
    # * Check out `crack_times_display` in the output
    # * Gives a score to the password, from 0 (terrible) to 4 (great)
    ec "$*" | zxcvbn --user-input "$pass_check_additional" | json-beautify
}
##
