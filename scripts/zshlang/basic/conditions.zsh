### BASH COMPATIBLE (Gets sourced by .shared.sh)
### Bash doesn't support aliases in scripts.
function iszsh {
    [[ -n $ZSH_VERSION ]]
}
function isZsh {
    iszsh
}

function isbash {
    [[ -n $BASH_VERSION ]]
}
function isBash {
    isbash
}
##
function isDarwin {
    # idk who is setting this, the OS?
    # `env -i zsh -fc 'echo $uname'` is empty
    ##
    if test -n "$uname" ; then
        [[ "$uname" == "Darwin" ]]
    else
        # works with bash, too, but the output can be a little different
        # `env -i bash -fc 'echo $OSTYPE'`
        ##
        [[ "$OSTYPE" == "darwin"* ]]
    fi
}
alias isD=isDarwin

function isArm {
    if test -z "$uname_m_cached" ; then
        uname_m_cached="$(uname -m)"
    fi

    [[ "$uname_m_cached" == "arm64" ]]
}

function isArmDarwin {
    isDarwin && isArm
}
function isAppleSilicon {
    isArmDarwin
}

function isLinux {
    if test -n "$uname" ; then
        [[ "$uname" == "Linux" ]]
    else
        [[ "$OSTYPE" == "linux"* ]]
    fi
}
alias isL=isLinux
alias isUbuntu=isLinux
##
function isLocal {
    # @darwinonly0
    isDarwin
}
function isServer {
    # @darwinonly0
    # This is actually isLinuxServer, but since macOS servers are rare, I have simplified
    isLinux
}
##
function isBorg {
    [[ -n "$JBRISH" ]]
}
function isJulia {
    isBorg "$@"
}
# isborg() { isBorg "$@" ; }
# isjulia() { isBorg "$@" ; }
##
function isEmacs {
    [[ -n "${NIGHT_EMACS_P}" ]]
}
##
function isBrish {
    [[ -n "${brish_server_index}" ]]
}

function isBrishOrg {
    [[ "$GARDEN_SESSION" == bsh ]]
}
##
function isTmux {
    test -n "$TMUX"
}
##
function isLilf {
    [[ "$(hostname)" == 'lilf.ir' ]]
}

function isZii {
    [[ "$(hostname)" == 'mail2.lilf.ir' ]]
}

function isMBP {
    local host="$HOST"
    if test -z "$host"; then # for bash
        host="$(hostname)"
    fi

    [[ "$host" == 'Fereidoons-MacBook-Pro.local' ]]
}

function isMB2 {
    local host="$HOST"
    if test -z "$host"; then # for bash
        host="$(hostname)"
    fi

    [[ "$host" == 'Feraidoons-MacBook-Air.local' ]]
}

function isGrayfur {
    local host="$HOST"
    if test -z "$host"; then # for bash
        host="$(hostname)"
    fi

    [[ "$host" == 'Parias-MacBook-Air.local' ]]
}

function isAeirya {
    local host="$HOST"
    if test -z "$host"; then # for bash
        host="$(hostname)"
    fi

    [[ "$host" == 'amadeus.local' ]]
}

function isGuest {
    isAeirya || isGrayfur
}

function isMe {
    isMB2 || isMBP || isLilf
}
##
function isKitty {
    if isLocal && isMe ; then
        return 0 # @surprise
    fi

    if true || isGuest || ( isLocal && ! isTmux ) ; then
        [[ "$TERM_PROGRAM" == kitty ]] || test -n "$KITTY_WINDOW_ID" || [[ "$TERM" == *kitty* ]]
        # the var KITTY_WINDOW_ID can be set incorrectly in tmux
        # we might have unexported KITTY_WINDOW_ID in auto-load/env.zsh, but we also export TERM_PROGRAM there:
        #  [[NIGHTDIR:zshlang/basic/auto-load/env.zsh::typeset +x KITTY_WINDOW_ID][auto-load/env.zsh::typeset +x KITTY_WINDOW_ID]]
        #
        # We can also use 'term-get', which works unless we are on mosh.
    fi
}
function iskitty {
    isKitty "$@"
}

function isAppleTerminal {
    [[ "$TERM_PROGRAM" == Apple_Terminal ]]
}

function isiTerm {
    [[ "$TERM_PROGRAM" == iTerm.app ]]
}
##
function isBicon {
    test -n "$BICON_MODE"
}

function isRtl {
    ##
    isBicon
    ##
    # isKitty || isAppleTerminal || isBicon
    ##
    # tmux can have its KITTY_WINDOW_ID empty. Instead of fixing that, let's just assume RTL
    # true
    ##
}
##
function isI {
    if test -n "$FORCE_NONINTERACTIVE" ; then
        return 1
    fi
    if test -n "$FORCE_INTERACTIVE" ; then
        return 0
    fi

    isIReally
}
function isIReally {
    if isBash ; then
        [[ $- == *i* ]]
    else
        [[ -o interactive ]]
    fi
}

function h-color-p-override {
    if test -n "$color_p" ; then
        if bool "$color_p" ; then
            ec y
        else
            ec n
        fi
    fi

    if test -n "$isColor_override" ; then
        if bool "$isColor_override" ; then
            ec y
        else
            ec n
        fi
    fi
}

function isColor {
    local color_p
    color_p="$(h-color-p-override)" @TRET
    if test -n "$color_p" ; then
        bool "$color_p"
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
    ## @alt
    # wget -q --spider http://google.com
    ##
    local c="${1:-3}"
    local i
    for i in {1..$c} ; do
        if h-isNet ; then
            return 0
        fi
    done

    return 1
}

function h-isNet() {
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
