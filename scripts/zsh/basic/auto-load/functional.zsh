function combine-funcs() {
    # Combine multiple functions into one named by $1; The result will run all functions with $@.
    local tmp321_string="function $1() { "
    for i in "${@:2}"
    do
        tmp321_string="$tmp321_string""$i "'"$@"; '
    done
    tmp321_string="$tmp321_string""}"
    # echo "$tmp321_string"
    eval "$tmp321_string"
}
