function pop-dn() {
    pbpaste | reval-copy tr -d '\n'
}
aliasfn p-dn pop-dn
