ashL() {
    local a=() i
    for i in "$@[2,-1]"
    do
        a+=(-L "${i}:localhost:${i}")
    done
    ash -NT "$1" "$a[@]"
}
