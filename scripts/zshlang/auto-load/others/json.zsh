##
function jqm {
    # --raw-output / -r: With this option, if the filter's result is a string then it will be written directly to standard output rather than being formatted as a JSON string with quotes. This can be useful for making jq filters talk to non-JSON-based systems.
    # --join-output / -j: Like -r but jq won't print a newline after each output.
    jq -re "$@[1,-2]" "$@[-1] // empty"
}

function jq-rtl {
    if isOutTty ; then
        reval-rtl jq --color-output "$@"
    else
        jq "$@"
    fi
}
##
function jq-quote() {
    local i
    i="$(in-or-args "$@")" @TRET # this trim-rights the input, which seems desirable
    ecn "$i" | jq --raw-input --slurp --null-input --compact-output 'inputs'
    ## tests:
    # `arrN h j k 'j m n "man"' " 'god'" |jq-quote `
    ##
}
##
json-beautify() {
    if isI ; then
        jq --color-output . | less
    else
        jq .
    fi
}
##
function arrJ-noquote() {
    local items=( "$@" )

    print -nr -- "[ ${(j.,.)items} ]"
}
function arrJ() {
    local items=( "$@" )

    ##
    jq --null-input '$ARGS.positional' --args "${items[@]}"
    ##
}
function arrJ-in() {
    # From https://github.com/stedolan/jq/issues/563
    jq --raw-input --null-input '[inputs | select(length>0)]'
}
##
