jqm() {
    jq -re "$@[1,-2]" "$@[-1] // empty"
}
