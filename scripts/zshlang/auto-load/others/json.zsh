##
function json5-to-json {
    cat-paste-if-tty |
        json5_to_json.py |
        cat-copy-if-tty
}
##
function jqm {
    # --raw-output / -r: With this option, if the filter's result is a string then it will be written directly to standard output rather than being formatted as a JSON string with quotes. This can be useful for making jq filters talk to non-JSON-based systems.
    # --join-output / -j: Like -r but jq won't print a newline after each output.
    json5-to-json |
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
function jq-len {
    jq -re '. | length'
}
##
function json-rm-keys-empty {
    #: This might only work on simple JSONs, IDK.
    ##
    jq '. |= map(with_entries(select(.value | . != "" and . != null)))'
    #: =map(x)= is equivalent to =[.[] | x]=
}
##
function jq-quote {
    local i
    i="$(in-or-args "$@")" @TRET # this trim-rights the input, which seems desirable
    ecn "$i" | jq --raw-input --slurp --null-input --compact-output 'inputs'
    ## tests:
    # `arrN h j k 'j m n "man"' " 'god'" |jq-quote `
    ##
}
##
function json-beautify {
    if isI ; then
        jq --color-output . | less
    else
        jq .
    fi
}

function json-beautify2 {
    : "also works with Python dictionaries"

    prettier --parser=json5
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
function json-listify-first-level {
    jq '.[] |= if type == "array" then . else [.] end'
}

function json-listify-first-level-py {
    json_listify_first_level.py
}
##
function json-stringify-atoms {
    json_stringify_atoms.py
}
##
