function pastebin {
    curl --progress-bar --form 'sprunge=<-' http://sprunge.us | cat-copy
    # @alt 'f:1=<-' ix.io
}
