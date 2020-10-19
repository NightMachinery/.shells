### BASH COMPATIBLE (Gets sourced by .shared.sh)
### Bash doesn't support aliases in scripts.
function iszsh() {
    [[ -n $ZSH_VERSION ]]
}
isZsh() { iszsh ; }
function isbash() {
    [[ -n $BASH_VERSION ]]
}
isBash() { isbash ; }
function isDarwin() {
    [[ "$uname" == "Darwin" ]]
}
alias isD=isDarwin
function isLinux() {
    [[ "$uname" == "Linux" ]]
}
alias isL=isLinux
function isMBP() {
    [[ "$(hostname)" == 'Fereidoons-MacBook-Pro.local' ]]
}
##
isI() {
    test -z "$FORCE_NONINTERACTIVE" && {
        test -n "$FORCE_INTERACTIVE" || [[ -o interactive ]] #[[ $- == *i* ]]
    }
}
##
function isOutTty() {
    [ -t 1 ]
    # -t fd True if file descriptor fd is open and refers to a terminal.
}
alias istty=isOutTty # NOTE: aliases are not fnswappable
alias isTty=isOutTty
##
alias isExpensive='[[ -z "$NIGHT_NO_EXPENSIVE" ]]'
alias isNotExpensive='[[ -n "$NIGHT_NO_EXPENSIVE" ]]'
##
function isDbg() {
    test -n "$DEBUGME"
}
alias isdbg=isDbg
function isNotDbg() {
    ! isDbg
}
##
