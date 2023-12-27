##
function xml2json {
    local json
    json="$(xml2json.py)" @RET
    ec "$json" | jq .
}
##
