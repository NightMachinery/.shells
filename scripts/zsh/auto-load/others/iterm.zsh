##
function iterm-boot() {
    # Alt: `lnrp iterm_focus.py ~/Library/Application\ Support/iTerm2/Scripts/AutoLaunch/` uses iterm's own python which doesn't have our Brish
    tmuxnewsh2 iterm_focus ITERM2_COOKIE=$(osascript -e 'tell application "iTerm2" to request cookie') reval-notifexit iterm_focus.py
}
##
it2prof() { echo -e "\033]50;SetProfile=$1\a" ; } # Change iterm2 profile. Usage it2prof ProfileName (case sensitive)
##
function iterm-session-active() {
    : "Needs iterm_focus.py running. Currently gets last active session, i.e., if you switch to Chrome, the active session won't be set to null. We can probably solve this, as there is a TERMINAL_WINDOW_RESIGNED_KEY event, but the current behavior seems better."
    # : "Doesn't work with tmux. iTerm's native tmux integration is also very invasive and not suitable for us. Setting ITERM_SESSION_ID manually by ourselves can solve this problem somewhat. It would work fine for ivy, but detaching and reattaching is hard to get right for it if at all."

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
