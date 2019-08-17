function prefix-files() {
    for file in "${@:2}"
    do
        mv "$file" "${file:h}/$1${file:t}"
    done
}
function rp() {
    test -e "$1" && realpath "$1" || ge_no_ec=y ge_no_hist=y ceer "$1" realpath
}
ensure-empty() {
    (silence eval 'comment *(D)') && {
        ecerr Directory "$(pwd)" not empty
        return 1
    } || return 0
}
typeset -ag pushf_stack
pushf() {
    # mkdir -p "$1"
    pushf_stack+="$(pwd)"
    cdm "$1"
}
popf() {
    cd "${pushf_stack[-1]}"
    pushf_stack=("${(@)pushf_stack[1,-2]}")
}
