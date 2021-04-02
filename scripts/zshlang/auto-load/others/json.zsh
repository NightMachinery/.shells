jqm() {
    # --raw-output / -r: With this option, if the filter's result is a string then it will be written directly to standard output rather than being formatted as a JSON string with quotes. This can be useful for making jq filters talk to non-JSON-based systems.
    # --join-output / -j: Like -r but jq won't print a newline after each output.
    jq -rje "$@[1,-2]" "$@[-1] // empty"
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
