###
## None of these work for me under tmux. icat-it and icat-py might work if the image's original data is sufficiently small. (I could never get them to work.)
aliasfn icat-it ~/.iterm2/imgcat
aliasfn ils-it ~/.iterm2/imgls
## https://github.com/olivere/iterm2-imagetools
function icat-go() {
    isI || return 0
    local h="${icat_go_height:-${icat_go_h:-600}}"
    
    "$GOBIN/imgcat" -height ${h}px "$@" # This will zoom, too, but that's actually good in most cases!
}
function ils() {
    isI || { exa -a ; return 0 }
    imgls -height 200px ${~imageglob} "$@"
}
## https://github.com/wookayin/python-imgcat
function icat-py() {
    isI || return 0
    imgcat --height 30 "$@" # This will zoom, too, but that's actually good in most cases!
}
##
function icat() {
    isI || return 0
    if test -z "$TMUX" ; then
        icat-go "$@"
    else
        icat-py "$@"
        ecerr "$0: Tmux not supported."
    fi
}
###
function iterm-boot() {
    # Alt: `lnrp iterm_focus.py ~/Library/Application\ Support/iTerm2/Scripts/AutoLaunch/` uses iterm's own python which doesn't have our Brish
    tmuxnewsh2 iterm_focus ITERM2_COOKIE=$(osascript -e 'tell application "iTerm2" to request cookie') reval-notifexit iterm_focus.py
}
##
it2prof() { echo -e "\033]50;SetProfile=$1\a" ; } # Change iterm2 profile. Usage it2prof ProfileName (case sensitive)
##
function iterm-session-active() {
    : "Needs iterm_focus.py running. This gives you the last active session. Use iterm-focus-is to see if iTerm is actually in focus or not."
    # : "Doesn't work with tmux. iTerm's native tmux integration is also very invasive and not suitable for us. Setting ITERM_SESSION_ID manually by ourselves can solve this problem somewhat. It would work fine for ivy, but detaching and reattaching is hard to get right for it if at all."
    # Alt for linux: See focus events http://invisible-island.net/xterm/ctlseqs/ctlseqs.html:
    # These need iTerm to immediately send the focus events when enabled, not waiting for a focus change. I tested it with `unset hi ; echo -ne '\e[?1004h\e[?1004l'; read -r -k 3 -t 0.5 hi ; typeset -p hi`; `hi` remained unset.
    # Alt for linux: xdotool https://unix.stackexchange.com/questions/480052/how-do-i-detect-whether-my-terminal-has-focus-in-the-gui-from-a-shell-script

    redis-cli --raw get iterm_active_session
}

function iterm-session-my() {
    if [[ "$ITERM_SESSION_ID" =~ '[^:]*:(.*)' ]] ; then
        ec "$match[1]"
    else
        return 1
    fi
}

function iterm-session-is-active() {
    [[ "$(iterm-session-active)" == "$(iterm-session-my)" ]]
}

function iterm-focus-get() {
    redis-cli --raw get iterm_focus
}

function iterm-focus-is() {
    [[ "$(iterm-focus-get)" == TERMINAL_WINDOW_BECAME_KEY ]]
}
