function isDarwin() { [[ "$(uname)" == "Darwin" ]] }
alias isD=isDarwin
function isLinux() { [[ "$(uname)" == "Linux" ]] }
alias isL=isLinux
isI() {
    ! test -z "$FORCE_INTERACTIVE" || [[ $- == *i* ]]
}
