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
if iszsh ; then
    function isGuest {
        [[ "$HOSTNAME" == 'amadeus.local' ]]
    }
else
    function isGuest {
        [[ "$(hostname)" == 'amadeus.local' ]]
    }
fi
##
function isKitty() {
    if isMBP ; then
        return 0 # @surprise
    fi

    if true || isGuest || ( isLocal && ! isTmux ) ; then
        test -n "$KITTY_WINDOW_ID"
        # the var can be set incorrectly in tmux
        # we have also unexported this var in auto-load/env.zsh
    fi
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
    if test -n "$FORCE_NONINTERACTIVE" ; then
        return 1
    fi
    if test -n "$FORCE_INTERACTIVE" ; then
        return 0
    fi

    isIReally
}
function isIReally() {
    if isBash ; then
        [[ $- == *i* ]]
    else
        [[ -o interactive ]]
    fi
}

function isColor() {
    if test -n "$isColor_override" ; then
        bool "$isColor_override"
        return $?
    fi

    isBrishOrg || isI
}

function isColorTty {
    isColor && isOutTty
}

function isColorErrTty {
    isColor && isErrTty
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
function isExpensive() {
    [[ -z "$NIGHT_NO_EXPENSIVE" ]]
}

function  isNotExpensive() {
    [[ -n "$NIGHT_NO_EXPENSIVE" ]]
}

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
function isNet() {
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
function isSudo() {
    # The $EUID environment variable (or `id -u`) holds the current user's UID. Root's UID is 0.

    [ "${EUID:-$(id -u)}" -eq 0 ]
}
function isRoot() {
    isSudo "$@"
}
##
