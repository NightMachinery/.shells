function blc() {
    doc brew link custom
    mkdir -p ~/bin/
    ln -s "$(brew --cellar "$1")"/**/"$2" ~/bin/"$3"
}
function bii() {
    brew reinstall "$@"
    ## Old: "DEPRECATED: Use brew reinstall"
    # brew bundle --file=/dev/stdin <<<"brew \"$1\" ${@:2}"
}
function brew-search() {
    brew search "$@"

    tts-glados1-cached 'brew search, complete'
}
alias bs='brew-search'
##
