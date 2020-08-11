function zir() {
    local dest="$(removeTrailingSlashes "$1")$2".zip
    \rm "$dest" &> /dev/null
    zip -r "$dest" "$1"
}
function unzip2dir() {
    local file="$1"

    unzip "$file" -d "${file:r}/"
}
reify unzip2dir
