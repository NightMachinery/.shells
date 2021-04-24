function zir() {
    local dest="$(removeTrailingSlashes "$1")$2".zip
    \rm "$dest" &> /dev/null
    zip -r "$dest" "$1"
}
function unzip2dir() {
    local file="$1" y="${unzip2dir_y:-y}"
    file="$(grealpath -e "$file")" @TRET

    local opts=()
    if bool $y ; then
        opts+='-y'
        # -y     Assume Yes on all queries
    fi

    assert 7z x $opts "$file" -o"${file:r}/" @RET
    ##
    # unzip "$file" -d "${file:r}/"
    ##
}
reify unzip2dir
aliasfn uzd unzip2dir
