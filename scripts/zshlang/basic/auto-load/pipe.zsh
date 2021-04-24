function pipe-get-nb() {
    # nonblocking
    command gdd bs=1G count=1 status=none iflag=nonblock 2>/dev/null
}
