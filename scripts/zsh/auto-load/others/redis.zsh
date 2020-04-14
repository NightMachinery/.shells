ensure-redis() {
    redis-cli --raw ping &> /dev/null || {
        ecerr '`redis-cli ping` failed. Please make sure redis is up.'
        return 1
    }
}
