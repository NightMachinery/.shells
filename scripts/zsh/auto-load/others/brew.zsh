blc() {
    doc brew link custom
    mkdir -p ~/bin/
    ln -s "$(brew --cellar "$1")"/**/"$2" ~/bin/"$3"
}
function bii() {
    brew bundle --file=/dev/stdin <<<"brew \"$1\" ${@:2}"
}
