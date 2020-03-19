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
mapg() {
    local args=("$@")
    local i res=''
    for i in "$@[2,-1]"
    do
        set -- "$i"
        res+="$(eval ec "$args[1]")"${mg_sep:-$'\n'}
    done
    ec "$res"
}
mapln() { mg_sep=$'\n' mapg "$@" }
mapnul() { mg_sep=$'\0' mapg "$@" }
revargs() {
	mdoc reverse args: "$0" '<function>' arguments... MAGIC
	# (O = reverse of the order specified in the next flag, a = normal array order).
	eval "$1" "$(gq "${(Oa)@[2,-1]}")"
}
inargs() {
	mdoc Feeds the function from stdin if no arguments supplied MAGIC
	# An enhancer
	eval "$1" "$(gq "$(in-or-args "${@:2}")")"
}
