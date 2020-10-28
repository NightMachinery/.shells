function jfic-patreon() {
    local l="$1"
    # example: https://www.patreon.com/theblankcanvas/posts\?filters\[tag\]\=A%20Cadmean%20Victory

    withchrome getlinks-c "$l" -e '/posts/' | uniq| tac | withchrome inargsf tl
}
