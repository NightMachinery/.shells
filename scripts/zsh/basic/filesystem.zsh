function rpargs() {
    local i args
    args=()
    for i in "$@"
             { test -e "$i" && args+="$(realpath "$i")" || args+= "$i" }
             ec "$args[@]"
}
