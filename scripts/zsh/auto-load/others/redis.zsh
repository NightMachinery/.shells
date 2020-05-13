###
ensure-redis() {
    (( ${+commands[redis-cli]} )) || {
        ecerr "redis-cli not found. Have you installed redis?"
        return 2
    }
    redis-cli --raw ping &> /dev/null || {
        ecerr '`redis-cli ping` failed. Please make sure redis is up.'
        return 1
    }
}
redism() {
    redis-cli --raw "$@"
}
