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
if iszsh ; then
    function isMBP() {
        [[ "$HOST" == 'Fereidoons-MacBook-Pro.local' ]]
    }
else
    function isMBP() {
        [[ "$(hostname)" == 'Fereidoons-MacBook-Pro.local' ]]
    }
fi
function isLocal() {
	# @darwinonly
	isDarwin
}
##
function isKitty() {
    test -n "$KITTY_WINDOW_ID"
}
iskitty() { isKitty "$@" ; }
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
isNet() {
    # wget -q --spider http://google.com
    
    ping -q -c 1 -W 1 8.8.8.8 &>/dev/null
}

alias mycountry='geo.bash -o country'
isIran() {
    # [[ "$(mycountry)" == Iran ]]

    ## faster
    ! ping -q -c 1 -W 1 facebook.com &>/dev/null #  faster when it succeeds
    # ! ping -q -c 1 -W 1 69.171.250.35 &>/dev/null # (facebook's ip) succeeds even in Iran
}
