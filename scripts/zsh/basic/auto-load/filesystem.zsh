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
cdm() {
    mkdir -p -- "$1" &&
        cd -P -- "$1"
}
function bottomdir() {
    { { [ -e "$1" ]  && ! [ -d "$1" ] } || [[ "$1" != */ ]] } && { ec "${1:h}"; } || { ec "$1"; } ;}
function cdd() {
    cd "$(bottomdir "$1")" }
trs() {
    local i
    for i in "$@"
    do
        [[ -e "$i" ]] && {
            ec Trying to remove "$i"
            trash -- "$i"
        }
    done
}
