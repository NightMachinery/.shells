jqm() {
    jq -re "$@[1,-2]" "$@[-1] // empty"
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
