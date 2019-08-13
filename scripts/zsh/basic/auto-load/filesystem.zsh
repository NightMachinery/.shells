function rpargs() {
    doc turns all existent paths in its args to absolute paths
    doc stdout: NUL separated args
    # dact arger "$@"|color 100 255 30
    local i args
    args=()
    for i in "$@"
    do
        test -e "$i" && args+="$(realpath --canonicalize-existing -- "$i")" || args+="$i"
    done
    re "printf '%s\0'" "$args[@]"
}
