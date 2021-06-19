function fanficfare2org {
    local url="$1"
    assert-args url @RET

    fanficfare --meta-only --json-meta "$url" | fanficfare2org.lisp
}
