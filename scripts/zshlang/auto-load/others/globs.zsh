# lots of glob vars defined in configvars
##
function isbinary() {
    # Usage: vfiles+=( $i/**/*(.D^+isbinary) )


    ## Method 1
    # https://unix.stackexchange.com/a/275691/282382
    # It searches for NUL bytes in the first buffer read from the file (a few kilo-bytes for a regular file, but could be a lot less for a pipe or socket or some devices like /dev/random). In UTF-8 locales, it also flags on byte sequences that don't form valid UTF-8 characters. It assumes LC_ALL is not set to something where the language is not English.
    
    # LC_MESSAGES=C ggrep -Hm1 '^' < "${1:-$REPLY}" | ggrep -q '^Binary'

    ## Method 2
    local mime="$(file -b --mime-type "${1:-$REPLY}")"
    ! [[ "$mime" =~ '^(text/|application/json)' ]]
}
function isBinary() {
    isbinary "$@"
}
function isnotbinary() {
    ! isbinary "$@"
}
function isNotBinary() {
    isnotbinary "$@"
}
