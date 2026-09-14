### Host identity and personal infrastructure predicates, split out of
### =conditions.zsh= so that the general half can join the public
### `basic.plugin.zsh' surface. Only =basic-full.zsh= loads this file.
###
### Nothing here belongs in a public plugin: most of it names this person's own
### machines. `isLocal'/`isRemote'/`isServer' must stay here in particular,
### because =setup/minimal_proxy/.shared.sh= deliberately hardcodes `isLocal'
### to false on the minimal servers -- it is sourced from =.zshenv= and a
### plugin loaded later from =.zshrc= would silently win.
###
### BASH COMPATIBLE, like =conditions.zsh=.
function isLocal {
    # @darwinonly0
    isDarwin
}

function isRemote {
    ! isLocal
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

    # [[ "$host" == 'Feraidoons-MacBook-Air.local' ]]
    [[ "$host" == 'mb2.local' ]]
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
    # if isLocal && isMe ; then
    #     return 0  #: @surprise
    # fi

    if isTmux ; then
        #: [[id:6f98aca5-a5a3-449e-833c-ba58627f1ad4][detect kitty terminal when inside tmux]]
        #:
        #: Prefer the XTVERSION reply over TERM. A client can claim `xterm-kitty`
        #: without being kitty (Termux is commonly set that way by hand, to
        #: unlock truecolor), and trusting TERM made us hand kitty-only
        #: capabilities to an emulator that garbles them.
        #: @see =docs/tmux-termux-truecolor.md=
        ##
        if tmux-client-termtype-supported-p ; then
            #: A terminal that does not answer the query is not kitty.
            [[ "$(tmux-client-termtype-get)" =~ '\bkitty\b' ]]
            return $?
        fi

        #: tmux <3.3 has no =#{client_termtype}=; fall back to the spoofable TERM.
        [[ "$(tmux-client-terminal-get)" =~ '\bkitty\b' ]]
        return $?
    fi

    if true ; then
        #: || isGuest || ( isLocal && ! isTmux )

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
##
function h-color-p-override {
    #: Be careful not to override =color_p= accidentally!
    ##
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
    local my_color_p
    my_color_p="$(h-color-p-override)" @TRET
    if test -n "$my_color_p" ; then
        bool "$my_color_p"
        return $?
    fi

    if isBorg ; then
        return 1
    fi

    isBrishOrg || isI
}

function isColorTty {
    local my_color_p
    my_color_p="$(h-color-p-override)" @TRET
    if test -n "$my_color_p" ; then
        bool "$my_color_p"
        return $?
    fi

    isColor && isOutTty
}

function isColorErrTty {
    local my_color_p
    my_color_p="$(h-color-p-override)" @TRET
    if test -n "$my_color_p" ; then
        bool "$my_color_p"
        return $?
    fi

    isColor && isErrTty
}
##
function true-color-p {
    # --- optional knobs (dependency injection via env) ---
    # If set truthy (y/yes/1), force success.
    local force_p="${true_color_p_force_p:-}"

    # --- environment inputs (declared local by convention) ---
    local colorterm="${COLORTERM:-}"
    local term="${TERM:-}"

    # Honor explicit force
    if bool "${force_p}" ; then
        return 0
    fi

    if isKitty || isiTerm ; then
        return 0
    elif isAppleTerminal ; then
        return 1
        #: @toFuture/1407 They might support True Color in the future.
    fi

    # 1) COLORTERM check (per termstandard/colors)
    if test -n "${colorterm}" ; then
        local ct_lc
        ct_lc="$(ec "${colorterm}" | tr '[:upper:]' '[:lower:]')"
        if [[ "${ct_lc}" == *"truecolor"* ]] || [[ "${ct_lc}" == *"24bit"* ]] ; then
            return 0
        fi
    fi

    # 2) terminfo capability check (RGB official, Tc is tmux extension)
    #    Use no flags to avoid non-portable options; parse with perl.
    if command -v -- infocmp >/dev/null 2>&1 ; then
        local ti
        ti="$(infocmp 2>/dev/null)" || true
        if test -n "${ti}" ; then
            if print -r -- "${ti}" | perl -0777 -ne 'exit 0 if /\bRGB\b|\bTc\b/; END { exit 1 }' ; then
                return 0
            fi
        fi
    fi

    # 3) Conservative TERM heuristics (minimal, only explicit truecolor terms)
    #    Many emulators still advertise xterm-256color, so we avoid guessing.
    if test -n "${term}" ; then
        local term_lc
        term_lc="$(ec "${term}" | tr '[:upper:]' '[:lower:]')"
        case "${term_lc}" in
            *-truecolor|tmux-truecolor)
                return 0
            ;;
        esac
    fi

    return 1
}
##
function isExpensive {
    [[ -z "$NIGHT_NO_EXPENSIVE" ]]
}

function isNotExpensive {
    [[ -n "$NIGHT_NO_EXPENSIVE" ]]
}

function isRcLoaded {
    test -n "$rcLoaded"
}
##
