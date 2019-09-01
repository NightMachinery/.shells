ebook-cover() {
    mdocu '[<ebook>=$jufile <output>=cover.jpg]
Saves the cover of the given ebook to <output>.' MAGIC
    local in="${1:-$jufile}" out="${2:-cover.jpg}"
    ebook-meta --get-cover "$out" "$in"
}
alias jec=ebook-cover
