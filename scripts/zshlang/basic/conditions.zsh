### General predicates: portable, self-contained, and part of the public
### `basic.plugin.zsh' surface. Host identity and personal infrastructure live
### in =conditions-personal.zsh=, which only =basic-full.zsh= loads.
###
### BASH COMPATIBLE: this parses and runs under bash too, so a minimal
### bootstrap can source it. It is *not* sourced by
### =setup/minimal_proxy/.shared.sh=, which keeps its own hand-written copies of
### a few of these; the comment that used to claim otherwise was stale.
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
function claude-code-p {
    #: Claude Code exports CLAUDECODE=1 (plus CLAUDE_CODE_SESSION_ID, CLAUDE_PID, ...) in every shell it spawns.
    #: AI_AGENT (e.g., "claude-code_2-1-220_agent") is also set by Claude Code 2.1+.
    ##
    [[ "${CLAUDECODE}" == 1 ]] ||
        [[ "${AI_AGENT}" == claude* ]]
}

function codex-p {
    #: Codex CLI exports CODEX_SANDBOX (e.g., "seatbelt") in its sandboxed shells.
    #: Unsandboxed shells still carry CODEX_THREAD_ID (or the older
    #: CODEX_SESSION_ID). Like the other agent markers, these are inherited.
    ##
    test -n "${CODEX_SANDBOX}" ||
        test -n "${CODEX_THREAD_ID}" ||
        test -n "${CODEX_SESSION_ID}" ||
        [[ "${AI_AGENT}" == codex* ]]
}

function antigravity-p {
    #: Google's Antigravity CLI (`agy`) exports ANTIGRAVITY_AGENT=1,
    #: ANTIGRAVITY_TRAJECTORY_ID and ANTIGRAVITY_CONVERSATION_ID (plus
    #: TERM=dumb, PAGER=cat) in the shells its run_command tool spawns. Read
    #: off the binary: `cortex/command/command.(*goRunner).Run', agy 1.1.28.
    #: It is not Gemini CLI and does not set GEMINI_CLI.
    ##
    test -n "${ANTIGRAVITY_AGENT}" ||
        test -n "${ANTIGRAVITY_TRAJECTORY_ID}" ||
        test -n "${ANTIGRAVITY_CONVERSATION_ID}" ||
        [[ "${AI_AGENT}" == (antigravity|agy)* ]]
}

function ai-agent-p {
    #: Is the current program being run by an AI agent (Claude Code, Codex, ...)?
    #: @warn These env vars are inherited by child processes, so this means "was started by an agent (or its descendants)". Not a security boundary; trivially spoofable.
    ##
    test -n "${AI_AGENT}" ||
        claude-code-p ||
        codex-p ||
        antigravity-p
}

function ai-agent-name {
    #: Prints which agent spawned this shell: claude, codex or agy. Fails when
    #: none did. Same caveats as [agfi:ai-agent-p].
    ##
    if claude-code-p ; then
        ec claude
    elif codex-p ; then
        ec codex
    elif antigravity-p ; then
        ec agy
    else
        return 1
    fi
}
##
function isTmux {
    test -n "$TMUX"
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
##
function isOutTty {
    [ -t 1 ]
    # -t fd True if file descriptor fd is open and refers to a terminal.
}
alias istty=isOutTty # NOTE: aliases are not fnswappable
alias isTty=isOutTty

function isErrTty {
    [ -t 2 ]
    # -t fd True if file descriptor fd is open and refers to a terminal.
}

function isInTty {
    [ -t 0 ]
    # -t fd True if file descriptor fd is open and refers to a terminal.
}
##
function isDbg {
    test -n "$DEBUGME"
}
alias isdbg=isDbg
alias isdebug=isDbg
alias isDebug=isDbg
alias 'debug-p'=isDbg
function isNotDbg {
    ! isDbg
}
##
function isNet {
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

function h-isNet {
    ##
    # local test_ip="${isNet_ip:-185.97.118.92}"
    local test_ip="${isNet_ip:-8.8.8.8}"

    if isDarwin ; then
        ping -q -c 1 -W 400 "${test_ip}" &>/dev/null
        # -W waittime in ms
    else
        ping -q -c 1 -W 1 "${test_ip}" &>/dev/null
        # -W waittime in s
    fi
}
##
function isSudo {
    # The $EUID environment variable (or `id -u`) holds the current user's UID. Root's UID is 0.

    [ "${EUID:-$(id -u)}" -eq 0 ]
}
function isRoot {
    isSudo "$@"
}
##
