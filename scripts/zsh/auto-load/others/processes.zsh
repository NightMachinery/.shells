function pk() {
    pgrep -i "$@"
    pkill -9 -i "$@"
}
