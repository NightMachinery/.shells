###
ensure-redis() {
    (( ${+commands[redis-cli]} )) || {
        ecerr "redis-cli not found. Have you installed redis?"
        return 2
    }
    redism ping &> /dev/null || {
        ecerr '`redis-cli ping` failed. Please make sure redis is up.'
        return 1
    }
}
redism() {
    redis-cli --raw "$@"
    local r=$?
    if (( r == 141 )) ; then
        local cmd="$(gq "$0" "$@")"
        local msg="$0: redis returned $r (is stdout a bad pipe?). Retrying : $cmd"
        ecerr $msg
        ectty $msg
        # eval "$cmd"
        return $?
    fi
    return $r
}
##
function redis-defvar() {
    local name="${1:?}"
    function "${name}_get"() { redism get "$name" }
    function "${name}_set"() { silent redism set "$name" "$@" }
    function "${name}_setnx"() { silent redism setnx "$name" "$@" }
    function "${name}_del"() { silent redism del "$name" }
}
##
