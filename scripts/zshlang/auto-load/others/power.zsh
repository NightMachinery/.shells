##
aliasfn displaysleep-darwin pmset displaysleepnow
aliasfn displaysleep displaysleep-darwin
##
#: These two are what hyper+shift+F1 / F2 are bound to in
#: `hammerspoon/core/window-media-bindings.lua`. They blank the screen rather
#: than merely dim it, so they go through [agfi:display-black-on], which knows
#: that DDC luminance 0 leaves an external panel visibly lit while IOKit
#: brightness 0 really does cut a built-in backlight.
##
typeset -g caffeinate_key_blackout='blackout'
#: The key these hold display sleep with; see [agfi:caffeinate-on]. Blanking the
#: screen is only useful while the machine stays up, but it must not drop an
#: assertion something else is relying on.

function brightness-off {
    : "usage: brightness-off [<selector>]
Selectors: see [agfi:h-brightness-select]."
    ##
    local sel="${1:-${brightness_display:-main}}"

    caffeinate-on "$caffeinate_key_blackout"
    display-black-on "$sel"
}

function brightness-on {
    : "usage: brightness-on [<selector>]
With no selector, restores every blanked display."
    ##
    caffeinate-off "$caffeinate_key_blackout"

    #: Restores whatever the levels were, rather than the fixed 0.435 this used
    #: to jump to.
    display-black-off "$1"
}

#: The keep-blank variants of the above. hyper+shift+F1 / F2 are bound to these
#: rather than the one-shot ones, because macOS undoes a plain blackout on its
#: own; see the header comment on [agfi:display-black-on-loop].
##
function brightness-off-loop {
    : "usage: [lo_s=<seconds>] brightness-off-loop [<selector>]
Selectors: see [agfi:h-brightness-select]."
    ##
    local sel="${1:-${brightness_display:-main}}"

    caffeinate-on "$caffeinate_key_blackout"
    display-black-on-loop "$sel"
}

function brightness-on-loop {
    : "usage: brightness-on-loop [<selector>]
Stops the keep-blank loop and restores. With no selector, restores every
blanked display."
    ##
    caffeinate-off "$caffeinate_key_blackout"

    display-black-off-loop "$1"
}

function h-blackout-release {
    : "Ends a blackout if one is up, and lets go of the display sleep it was
holding. Cheap enough to call from a hook on every wake and every unlock.

Waking ends a blackout by definition: the machine only got here by sleeping, so
whatever the black screen was for is over. Left alone,
[agfi:display-black-on-loop] would re-assert brightness 0 every few seconds at a
login screen the brightness keys can no longer fix."
    ##
    if display-black-loop-p || display-black-p ; then
        brightness-on-loop
        return $?
    fi

    #: Nothing is blanked, but our key can outlive the blackout that took it (a
    #: garden restart, a crash). Nothing else holds display sleep on our behalf,
    #: so releasing it here is safe.
    caffeinate-off "$caffeinate_key_blackout"
}

#: See the note in system.zsh on why only this family gets selector suffixes.
for h_db_fn in brightness-off brightness-on ; do
    for h_db_sel in main all internal external ; do
        h_aliasfn "${h_db_fn}-${h_db_sel}" "${h_db_fn}" "${h_db_sel}"
        h_aliasfn "${h_db_fn}-${h_db_sel}-loop" "${h_db_fn}-loop" "${h_db_sel}"
    done
done
unset h_db_fn h_db_sel

##
#: Display sleep is held per key: one `caffeinate -d' per key, each in its own
#: tmux session. Nothing counts the holders -- "every key has released" is
#: exactly "no such process is left", which the kernel already tracks, so there
#: is no refcount to go stale across a reboot or a crashed holder, and no way
#: for our bookkeeping to disagree with the actual assertion.
#:
#: Holders do not interfere. An assertion is per-process, and the system-wide
#: status `pmset -g assertions' prints is the OR over every holder listed under
#: it, so the display may sleep again only once the last one exits.
#:
#: @warn None of this can defeat closing the lid. That is the clamshell path,
#: not idle sleep, and no assertion applies to it. See ./docs/caffeinate.md.
##
typeset -g caffeinate_session_prefix='caffeinate-'

function h-caffeinate-session {
    : "usage: h-caffeinate-session [<key>]
The tmux session name holding <key>'s assertion."
    ##
    local key="${1:-${caffeinate_key:-misc}}"

    #: tmux rejects `:' and `.' in session names.
    key="$(str2tmuxname "$key")" @TRET
    key="${key:-misc}"

    ec "${caffeinate_session_prefix}${key}"
}

function caffeinate-on {
    : "usage: [caffeinate_key=<key>] caffeinate-on [<key>]
Prevents display sleep on behalf of <key>. [agfi:caffeinate-off] releases only
that key; the display can sleep again once the last key has let go."
    ##
    local session
    session="$(h-caffeinate-session "$@")" @TRET

    #: Not `tmuxnewsh2' blind: [agfi:tmuxnew] kills a session of the same name
    #: before creating it, so asking twice for a key we already hold would
    #: restart its assertion rather than do nothing.
    if tmux-alive-p "$session" ; then
        return 0
    fi

    reval-ecgray tmuxnewsh2 "$session" reval-ec caffeinate -d
}

function caffeinate-off {
    : "usage: [caffeinate_key=<key>] caffeinate-off [<key>]
Releases <key>. Any other key keeps the display awake; see
[agfi:caffeinate-holders]."
    ##
    local session
    session="$(h-caffeinate-session "$@")" @TRET

    if tmux-alive-p "$session" ; then
        reval-ecgray tmux kill-session -t "=${session}"
    fi

    local rest
    rest="$(caffeinate-holders)" || rest=''
    if test -n "$rest" ; then
        ecgray "$0: display sleep still held by: ${(j:, :)${(@f)rest}}"
    fi
}

function caffeinate-holders {
    : "The keys currently holding display sleep off, one per line."
    ##
    local out
    out="$(tmux list-sessions -F '#{session_name}' 2>/dev/null)" || return 0

    local line
    for line in "${(@f)out}" ; do
        if [[ "$line" == "${caffeinate_session_prefix}"* ]] ; then
            ec "${line#${caffeinate_session_prefix}}"

        elif [[ "$line" == caffeinate ]] ; then
            #: The unkeyed session name this scheme replaced. It can still be
            #: around from before, and it is still asserting. Only
            #: [agfi:caffeinate-off-all] can release it.
            ec legacy
        fi
    done
}

function caffeinate-p {
    : "Whether anything is holding display sleep off."
    ##
    test -n "$(caffeinate-holders)"
}

function caffeinate-off-all {
    : "Releases every key, including the unkeyed session this predates."
    ##
    local out
    out="$(tmux list-sessions -F '#{session_name}' 2>/dev/null)" || return 0

    local line
    for line in "${(@f)out}" ; do
        [[ "$line" == "${caffeinate_session_prefix}"* || "$line" == caffeinate ]] || continue

        reval-ecgray tmux kill-session -t "=${line}"
    done
}
##
function display-off-brightness {
	local dur="${1:-1}"

	lo_s="$dur" loop brightness-set 0
}

function display-off {
	local after="${1:-0}"

	sleep-neon "$after"
    displaysleep
	#: causes display to go to sleep immediately.
}
alias lock-screen-user display-off
##
function sleepnow {
    sleep "${1:-7}"
    #: [[id:f8ad0757-23ae-4ab2-b046-06531e97bc13][Macbook not sleeping under Big Sur · Issue #2519 · pqrs-org/Karabiner-Elements]]
    #: Some amount of waiting before triggering the sleep is needed to avoid Karabiner canceling the sleep event.
    #: This is also useful without karabiner, as we may inadvertently generate some user "activity."

    reval-ec pmset sleepnow
}

function sleepforce {
    lo_s=60 lo_p=${1:-~/tmp/.sleepforce} loop sleepnow 10
}

function sleep-if-underloaded {
    while (( $(load5) >= "${1:-7}" ))
    do
        #: Load is too high, NOT sleeping
        sleep 150
    done

    ecdate "sleeping with load5=$(load5)"
    sleepforce
}
##
function battery-low-power-mode-enable {
    #: [[https://apple.stackexchange.com/questions/452488/how-can-i-set-the-low-power-mode-to-only-on-battery-programmatically][macos - How can I set the low power mode to "Only On Battery" programmatically? - Ask Different]]
    ecgray "@seeAlso powersaving-on"
    ##

    sudo pmset -b lowpowermode 1
    #: The -a, -b, -c, -u flags determine whether the settings apply to battery ( -b ), charger (wall power) ( -c ), UPS ( -u ) or all ( -a ).
}

function battery-low-power-mode-disable {
    sudo pmset -a lowpowermode 0
}

function battery-low-power-mode-p {
    local s
    s="$(pmset -g | rget 'lowpowermode\s+(\d+)')" @TRET

    (( s == 1 )) #: 1: lowpowermode 0: off
}

redis-defvar powersaving_status
powersaving_apps=(chrome Insiders bettertouch ActivityWatch OBS) # aw-watcher aw-server iterm tmux hammersp Notion Finder scsynth java podcast Telegram
# seems that -SIGSTOP is useless for scsynth
# btt starts itself up again after a minute or two (even with sudo kill)
# emacs daemon can't handle SIGSTOP
function powersaving-off {
    # ffkill -SIGCONT $powersaving_apps
    pgrep -i "${(j.|.)powersaving_apps}" | inargsf sudo kill -SIGCONT

    # awaysh /Applications/BetterTouchTool.app/Contents/MacOS/BetterTouchTool
    # open /Applications/BetterTouchTool.app

    # proxy-on
    # wgd
    ##
    powersaving_status_set off
    powersaving-widget-refresh
}

function powersaving-on {
    # ffkill -SIGSTOP $powersaving_apps
    pgrep -i "${(j.|.)powersaving_apps}" | inargsf sudo kill -SIGSTOP
    ##
    powersaving_status_set on
    # powersaving-widget-refresh
}

function powersaving-toggle {
    if powersaving-is ; then
        powersaving-off
    else
        powersaving-on
    fi
}
function powersaving-is() {
    [[ "$(powersaving_status_get)" == on ]]
}
powersaving_widget_uuid=E366290A-FC5D-4913-B068-CE9198F0511B
powersaving_widget_on="🔋"
powersaving_widget_off="💈"
# powersaving_widget_off="🔌"
function powersaving-widget() {
    if powersaving-is ; then
        ec $powersaving_widget_on
    else
        ec $powersaving_widget_off
    fi
}
powersaving-widget-refresh() { btt-update $powersaving_widget_uuid "$(powersaving-widget)" }
##
