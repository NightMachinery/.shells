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
function isLilf() {
    [[ "$(hostname)" == 'lilf.ir' ]]
}
function isZii() {
    [[ "$(hostname)" == 'mail2.lilf.ir' ]]
}
function isLocal() {
    # @darwinonly0
    isDarwin
}
function isServer() {
    # @darwinonly0
    # This is actually isLinuxServer, but since macOS servers are rare, I have simplified
    isLinux
}
##
function isBorg() {
    [[ -n "$JBRISH" ]]
}
isJulia() { isBorg "$@" ; }
# isborg() { isBorg "$@" ; }
# isjulia() { isBorg "$@" ; }
##
function isBrish() {
    [[ -n "${brish_server_index}" ]]
}

function isBrishOrg() {
    [[ "$GARDEN_SESSION" == bsh ]]
}
##
function isTmux() {
    test -n "$TMUX"
}
##
function isKitty() {
    true # @surprise
    
    # test -n "$KITTY_WINDOW_ID"
    # the var can be set incorrectly in tmux
}
iskitty() { isKitty "$@" ; }
function isAppleTerminal() {
    [[ "$TERM_PROGRAM" == Apple_Terminal ]]
}
function isiTerm() {
    [[ "$TERM_PROGRAM" == iTerm.app ]]
}
##
function isBicon() {
    test -n "$BICON_MODE"
}
function isRtl() {
    # isKitty || isAppleTerminal || isBicon
    ##
    # tmux can have its KITTY_WINDOW_ID empty. Instead of fixing that, let's just assume RTL
    true
    ##
}
##
function isI() {
    test -z "$FORCE_NONINTERACTIVE" && {
        test -n "$FORCE_INTERACTIVE" || [[ -o interactive ]] #[[ $- == *i* ]]
    }
}
function isIReally() {
    [[ -o interactive ]]
}

function isColor() {
    isBrishOrg || isI
}
##
function isOutTty() {
    [ -t 1 ]
    # -t fd True if file descriptor fd is open and refers to a terminal.
}
alias istty=isOutTty # NOTE: aliases are not fnswappable
alias isTty=isOutTty

function isErrTty() {
    [ -t 2 ]
    # -t fd True if file descriptor fd is open and refers to a terminal.
}

function isInTty() {
    [ -t 0 ]
    # -t fd True if file descriptor fd is open and refers to a terminal.
}
##
alias isExpensive='[[ -z "$NIGHT_NO_EXPENSIVE" ]]'
alias isNotExpensive='[[ -n "$NIGHT_NO_EXPENSIVE" ]]'
function isRcLoaded() {
    test -n "$rcLoaded"
}
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
    ##
    if isDarwin ; then
        ping -q -c 1 -W 400 8.8.8.8 &>/dev/null
        # -W waittime in ms
    else
        ping -q -c 1 -W 1 8.8.8.8 &>/dev/null
        # -W waittime in s
    fi
}
##
