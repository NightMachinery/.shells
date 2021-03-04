function objc-compile() {
    local f="${1:?}"

    local out="$HOME/bin/${f:t:r}"
    reval-ec clang -framework Carbon -framework Foundation "$f" -o "$out"
}
