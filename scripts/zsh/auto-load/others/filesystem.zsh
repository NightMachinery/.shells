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
function rename-numbered() {
    mdocu "rfp_dry=<dry-run?> <to-dir> <file> ..." MAGIC
    local c=1
    local i
    for i in "${@:2}"
             {
                 local dest="$1/$(printf '%03d' $c) ${i:t}"
                 echo "$i to "  $dest
                 [[ -z "$rfp_dry" ]] && mv "$i" $dest
                 c=$(($c + 1))
             }
}
