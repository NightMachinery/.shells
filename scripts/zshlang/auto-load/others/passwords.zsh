passgen() {
    # @alt passgen.go
    reval-copy openssl rand -base64 "${1:-16}"
}
##
pass-check() {
    # * pass_check_additional: user data to be added to the dictionaries that are tested against (name, birthdate, etc)
    # * Check out `crack_times_display` in the output
    # * Gives a score to the password, from 0 (terrible) to 4 (great)
    ec "$*" | zxcvbn --user-input "$pass_check_additional" | json-beautify
}
##
