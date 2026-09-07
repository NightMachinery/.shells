function kitty-sockets-list {
    #: Every kitty remote-control socket belonging to a kitty that is still
    #: alive, one path per line.
    #:
    #: `listen_on' puts kitty's pid in the socket's name (see
    #: =configFiles/kitty/kitty.conf=), so a socket left behind by a kitty that
    #: crashed is *identifiable* rather than merely ambiguous. That is the whole
    #: reason nothing here deletes anything: a stale socket costs a `pgrep'
    #: lookup, whereas deleting one you misjudged costs a running kitty its
    #: remote control until it is restarted, unrecoverably.
    #:
    #: `pgrep -x', never `-f': `-f' matches whole command lines, including our
    #: own.
    #:
    #: See docs/unix-sockets.md.
    ##
    local dir="${NIGHT_SOCKETS_DIR:-${HOME}/.local/state}"

    #: `(N)' so no match yields nothing rather than an error. Do NOT add `.':
    #: these are sockets, not regular files.
    local -a socks
    socks=( ${~${kitty_sockets_list_glob:-${dir}/kitty-*.sock}}(N) )
    (( ${#socks} )) || return 0

    local -a live
    live=( ${(f)"$(command pgrep -x kitty)"} )

    local s pid
    for s in "${socks[@]}" ; do
        #: `kitty-548.sock' -> `548'
        pid="${${s:t}#kitty-}"
        pid="${pid%.sock}"

        if (( ${live[(Ie)${pid}]} )) ; then
            ec "${s}"
        fi
    done
}

function kitty-socket-pid {
    #: kitty's pid, read out of the socket path that names it:
    #: `unix:/Users/evar/.local/state/kitty-548.sock' -> `548'.
    #:
    #: The pid is in the name because `listen_on' puts it there; see
    #: =configFiles/kitty/kitty.conf=. Knowing the owning kitty is what lets a
    #: registry key survive a kitty restart without ever matching the previous
    #: instance's windows.
    #: Usage: kitty-socket-pid <socket>
    ##
    local sock="${1}"
    test -n "${sock}" || return 1

    #: Greedy to the last `-', so dashes anywhere in the directory are
    #: harmless, then drop the suffix.
    local pid="${${sock##*-}%.sock}"

    #: `<->' is any run of digits: refuse to hand back half a filename.
    [[ "${pid}" == <-> ]] || return 1

    ec "${pid}"
}

function kitty-socket-get {
    #: The one live kitty's remote-control socket, as `unix:<path>'. Prints the
    #: reason to stderr and to $kitty_socket_get_err when there is not exactly
    #: one, so a caller wiring this into a notification can say *which* way it
    #: failed instead of a single unhelpful "no socket".
    #:
    #: `KITTY_LISTEN_ON' is trusted only while it still points at a socket that
    #: exists: BrishGarden's shells outlive kitty, so the garden holds whatever
    #: value was in the environment the day it started, and that goes stale the
    #: moment kitty restarts.
    ##
    unset kitty_socket_get_err

    if [[ "${KITTY_LISTEN_ON}" == unix:* ]] && test -e "${KITTY_LISTEN_ON#unix:}" ; then
        ec "${KITTY_LISTEN_ON}"
        return 0
    fi

    local -a socks
    socks=( ${(f)"$(kitty-sockets-list)"} )
    socks=( ${socks:#} )

    if (( ${#socks} == 1 )) ; then
        ec "unix:${socks[1]}"
        return 0
    fi

    #: Derived from the glob that was actually used, so an overridden glob
    #: cannot make the message name a directory we never looked in.
    local dir="${${kitty_sockets_list_glob:-${NIGHT_SOCKETS_DIR:-${HOME}/.local/state}/kitty-*.sock}:h}"

    if (( ${#socks} == 0 )) ; then
        local -a live
        live=( ${(f)"$(command pgrep -x kitty)"} )
        live=( ${live:#} )

        if (( ${#live} == 0 )) ; then
            kitty_socket_get_err="kitty is not running"
        else
            #: The failure that is invisible without being told: `listen_on' is
            #: startup-only ("Changing this option by reloading the config is
            #: not supported"), and an unlinked socket path cannot be re-linked,
            #: so kitty keeps the bound inode while every client gets ENOENT.
            #: Reloading the config will not help. Only a restart will.
            kitty_socket_get_err="kitty is running (pid ${live[1]}) but has no socket in ${dir/#${HOME}/~}; restart kitty, a config reload cannot re-create it"
        fi
    else
        kitty_socket_get_err="several live kitty sockets in ${dir/#${HOME}/~}: ${(j:, :)${socks[@]:t}}"
    fi

    ecerr "$0: ${kitty_socket_get_err}"
    return 1
}

function kitty-remote() {
    : "Invoke with no args to enter the kitty shell"

    if isKitty && ! isTmux ; then
        kitty @ "$@"
    else
        local s i ret=1
        s=( ${(f)"$(kitty-sockets-list)"} )
        s=( ${s:#} )
        if (( $#s >= 1 )) ; then
            for i in ${s[@]} ; do
                if kitty @ --to unix:${i} "$@" ; then
                    ret=0
                fi
                #: Deliberately no cleanup of sockets that do not answer. This
                #: used to `trs' them, and a `kitty @' that failed for any other
                #: reason -- a bad subcommand, a kitty too busy to reply -- took
                #: a live socket with it. [agfi:kitty-sockets-list] already
                #: filters by pid, so there is nothing left to collect.
            done
        fi

        if (( ret != 0 )) ; then
            ecerr "$0: could not send commands to any Kitty instance"
        fi
        return $ret
    fi
}
function kitty-send() {
    local m="${kitty_send_match}"
    in-or-args2 "$@"

    ec "${inargs[@]}" | kitty-remote send-text --match "$m" --stdin
    # https://sw.kovidgoyal.net/kitty/remote-control.html#kitty-send-text
}
function kitty-C-c() {
    kitty-send $'\C-c' #$'\n''reset'
}
@opts-setprefixas kitty-C-c kitty-send
function kitty-esc() {
    local m="${kitty_send_match}"
    # kitty-send $'\^['
    # kitty-send "$(printf '\x1b')"
    printf -- '\x1b\x1b\x1b\x1b\x1b\n' | kitty-remote send-text --match "$m" --stdin
}
@opts-setprefixas kitty-esc kitty-send
##
function kitty-tab-activate() {
    local i="${1:-5}"

    kitty-remote focus-tab --match="id:${i}"
    # https://sw.kovidgoyal.net/kitty/remote-control.html#kitty-focus-tab"
}
##
function kitty-tab-get {
    local id="${1:-$KITTY_WINDOW_ID}"
    if test -z "$id" ; then
        if isTmux ; then
            ecgray "$0: No 'id' supplied. tmux detected; Ignoring."
            return 1
        else
            assert-args id @RET
        fi
    fi

    kitty-remote ls | jq -r --arg wid "$id" '.[] | select(.is_focused == true) | .tabs[] | select(.windows[0].id == ($wid | tonumber))'
}

function kitty-tab-get-emacs() {
    kitty-remote ls | {
        # jq -r '.[] | select(.is_focused == true) | .tabs[] | select(.windows[0].foreground_processes[0].cmdline[0] | contains("emacs") ) | .id'
        ##
        jq -r '.[] | select(.is_focused == true) | .tabs[] | select(.windows[0].cmdline[]  | (contains("emc-gateway") and (contains("withemc") | not)) ) | .id'
    }
}
##
# if isIReally && isKitty ; then
#     typeset -g kitty_emacs_id="$(kitty-tab-get-emacs | ghead -n 1)"
# fi
function kitty-emacs-focus {
    local id=5

    ## cached for perf reasons: (the cache needs to invalidated each time 'kemc' is called
    if isDeus || test -z "$kitty_emacs_id" ; then
        id="$(kitty-tab-get-emacs | ghead -n 1)" @TRET
        typeset -g kitty_emacs_id="$id"
    else
        id="$kitty_emacs_id"
    fi
    ##

    if ! kitty-tab-activate "$id" ; then
        if ! isDeus ; then
            deus "$0" "$@"
        fi
    fi
    ## perf:
    # `time (kitty-tab-get-emacs | ghead -n 1)` 0.2s
    # `time2 kitty-emacs-focus` 0.50343608856201172
    # `time2 kitty-tab-activate 5 ` -> 0.165s
    ##
}

function kitty-tab-is-focused {
    local res
    res="$(serr kitty-tab-get)" @RET

    [[ "$(ec "$res" | jq -r '.is_focused')" == 'true' ]]
}
##
function kitty-emacs-focused-p {
    kitty-remote ls --match-tab 'state:focused' --match 'state:focused' | jq -e '
      any(.[]?.tabs[]?.windows[]?;
        (
          (
            [ .foreground_processes[]?.cmdline[]? ]
            | map(tostring)
            | any(test("emacs|emc-gateway"; "i"))
          )
          # or
          # ((.title // "") | test("emacs"))
        )
      )
    ' >/dev/null
}

function kitty-tab-codex-p {
    #: true (0) if the *focused window in the focused tab* is running codex
    #: kitty doesn't see through SSH (or perhaps even tmux), so we fallback on checking for TTY titles.
    #: If the TTY title starts with `⚡`, it's running Codex.
    ##
    kitty-remote ls --match-tab 'state:focused' --match 'state:focused' | jq -e '
      any(.[]?.tabs[]?.windows[]?;
        (
          (
            [ .foreground_processes[]?.cmdline[]? ]
            | map(tostring)
            | any(test("(^|[[:space:]/])codex([[:space:]]|$)"; "i"))
          )
          or
          ((.title // "") | test("^⚡"))
        )
      )
    ' >/dev/null
}
##
# redis-defvar kitty_focused
function kitty-is-focused {
    ##
    #: If we have fixed [agfi:frontapp-get], let's switch back to it. It's more reliable.
    #: But this is also slower, so I am keeping the second option.
    # [[ "$(frontapp-get)" == 'net.kovidgoyal.kitty' ]] ; return $?
    ##
    [[ "$(kitty-remote ls | jq -r '.[] | .is_focused')" == true ]]
    ##
    # @Redis
    # [[ "$(kitty_focused_get)" == 1 ]]
    ##
}
##
function kitty-launch-emc {
    kitty-remote launch '--type=tab' "${commands[zsh]}" -c "proxy_disabled=$proxy_disabled fnswap isColor true retry-limited 2 $@ emc-gateway"
    # The retry is to work around the recent emacs/doom issue that kills the starting frame (and sometimes all the frames, when doom themes are used).
    ##
    # This doesn't work, as somehow our config is not loaded. It will work if there is already a server running on EMACS_SOCKET_NAME though
    # kitty @ launch '--type=tab' bash -c 'TERM=xterm-emacs EMACS_SOCKET_NAME="$EMACS_SOCKET_NAME" ALTERNATE_EDITOR= emacsclient -t ; sleep 10'
    ##
}
alias kemc='kitty-launch-emc'

aliasfn withemc1 'EMACS_SOCKET_NAME="$EMACS_ALT1_SOCKET_NAME" emacs_night_server_name="$EMACS_ALT1_SOCKET_NAME"' reval-env
function kemc1 {
    kitty-launch-emc withemc1
}
##
function kitty-launch-icat {
    ## use this for troubleshooting:
    # kitty @ launch --type=tab env PATH="$PATH" "$(which wait4user.sh)" "$(which kitty)" +kitten icat "$@"
    # @raceCondition sometimes this does not work, no idea why
    ##
    local f fs=()
    for f in $@ ; do
        fs+="$(grealpath -e -- "$f")" @TRET
    done

    # @see icat-kitty-single
    revaldbg serrdbg kitty-remote launch --type=overlay env PATH="$PATH" "$(which kitty)" +kitten icat --hold --place "${COLUMNS}x${LINES}@0x0" --scale-up "$fs[@]"
    ##
}
##
function kitty-theme {
    # the theme names should be stripped of '.conf' before being fed to kitty-theme, and they should not be in abs paths:
    # `kitty-theme --test Zenburn `
    ##
    ensure-dep-kitty-theme

    command kitty-theme -c $NIGHTDIR/configFiles/kitty/kitty_theme_changer.conf.py "$@"
}

function kitty-theme-setup {
    #: `kitty-theme-setup Spring Dumbledore`
    ##
    local light="${1:-night-solarized-light}"
    local dark="${2}"
    dark="${dark:-Solarized_Dark_-_Patched}"
    # dark="${dark:-ayu}"

    reval-ecgray kitty-theme --setl "$light"

    reval-ecgray kitty-theme --setd "$dark"

    kitty-theme-reload
}

function kitty-theme-toggle {
    : "needs kitty-theme-setup to have been run"

    kitty-theme --toggle --live
}

function kitty-theme-test {
    : "Use 'kitty-theme-setup' to change the themes for all sessions."

    local theme q="$(fz-createquery "$@")"

    local dir
    dir=~/.config/kitty/kitty-themes/themes/

    theme="$(fd --extension conf . "$dir" | dir-rmprefix "$dir" | command sd '\.conf$' '' | fz)" @RET

    reval-ec kitty-theme --test "$theme"
}

function kitty-theme-reload {
    kitty-theme --live
}
##
##
function kitty-panel-ensure {
    #: kitty, running, with every tab inside its one panel OS window. Prints
    #: the remote-control socket as `unix:<path>'.
    #:
    #: We only ever reach kitty through hyper+z (see
    #: =hammerspoon/core/window-media-bindings.lua=), and what the hotkey must
    #: show is *all* the tabs, over whatever is in front, fullscreen apps
    #: included. A normal OS window cannot do that: macOS will not put one over
    #: a fullscreen space, and Hammerspoon cannot move one there either (a
    #: forced `hs.spaces.moveWindowToSpace' into a fullscreen space returns true
    #: and does nothing). A *panel* window can, and kitty can create one for
    #: itself with `launch --type=os-panel'. kitty's own quick-access and panel
    #: kittens do the same thing but run as a second app bundle
    #: (net.kovidgoyal.kitty-quick-access), which the rest of our code does not
    #: know; this keeps the one instance, the one bundle ID, and the one socket.
    #:
    #: The panel is recognised by its window class, `kitty-panel', which `ls'
    #: reports as `wm_class' for the OS window's whole life. Tabs living in any
    #: other OS window (the startup session opens in a normal one; scripts
    #: sometimes open another) are moved in with `detach-tab', in order, and
    #: the shell tab the panel was born with is closed once real tabs have
    #: arrived. An OS window left without tabs closes itself, and
    #: `macos_quit_when_last_window_closed' does not fire because the panel is
    #: still there.
    #:
    #: When kitty is not running it is started through Launch Services, like
    #: the Dock does, minimized so the session's normal window does not drag
    #: the display to the desktop before its tabs are moved.
    ##
    local sock
    sock="$(kitty-socket-get 2>/dev/null)" || {
        command open -b net.kovidgoyal.kitty --args --start-as minimized || return $?

        local i
        for i in {1..80} ; do
            sleep 0.25
            if sock="$(kitty-socket-get 2>/dev/null)" && kitty @ --to "${sock}" ls >/dev/null 2>&1 ; then
                break
            fi
            sock=
        done
        if test -z "${sock}" ; then
            ecerr "$0: kitty did not come up within 20 seconds"
            return 1
        fi
    }

    local ls_json
    ls_json="$(kitty @ --to "${sock}" ls)" || return $?

    local panel_q='[.[] | select(.wm_class == "kitty-panel")][0] | .tabs[0].id // empty'
    local panel_tab fresh_tab=
    panel_tab="$(command jq -r "${panel_q}" <<<"${ls_json}")"
    if test -z "${panel_tab}" ; then
        #: `edge center' anchors to all four edges, so it covers the display.
        #: `layer top' is the lowest layer that still floats above fullscreen
        #: windows (measured over a fullscreen Brave). Not `overlay': that sat
        #: above Handy's speech-to-text overlay too, which then could not be
        #: seen while dictating into kitty.
        kitty @ --to "${sock}" launch --type=os-panel \
            --os-panel edge=center --os-panel layer=top \
            --os-panel focus-policy=on-demand \
            --os-window-class kitty-panel --dont-take-focus >/dev/null || return $?
        ls_json="$(kitty @ --to "${sock}" ls)" || return $?
        panel_tab="$(command jq -r "${panel_q}" <<<"${ls_json}")"
        fresh_tab="${panel_tab}"
        if test -z "${panel_tab}" ; then
            ecerr "$0: created a panel but cannot find it in 'kitty @ ls'"
            return 1
        fi
    fi

    #: Nothing here may focus anything. While the panel is hidden macOS keeps
    #: it on the desktop space only, and activating kitty in that state (which
    #: `focus-tab' and `focus-window' do) can switch the display to the desktop
    #: before `show' has had the chance to join the current space. That was the
    #: "hyper+z from a fullscreen Brave lands on the desktop" bug. Focusing is
    #: kitty-panel-show's job, after the show.
    local -a stray
    stray=( ${(f)"$(command jq -r '.[] | select(.wm_class != "kitty-panel") | .tabs[].id' <<<"${ls_json}")"} )
    stray=( ${stray:#} )

    local t
    for t in "${stray[@]}" ; do
        kitty @ --to "${sock}" detach-tab --match "id:${t}" --target-tab "id:${panel_tab}" >/dev/null ||
            ecerr "$0: could not move tab ${t} into the panel"
    done

    if test -n "${fresh_tab}" && (( ${#stray} > 0 )) ; then
        kitty @ --to "${sock}" close-tab --match "id:${fresh_tab}" >/dev/null
    fi

    ec "${sock}"
}

function kitty-panel-show {
    #: Shows the panel, with keyboard focus in its active window. Idempotent.
    #: Showing a panel does not focus it by itself (measured), hence the
    #: explicit focus-window on the active tab's active window. The order is
    #: load-bearing: show first, so the panel has joined the current space,
    #: then focus. See the note in kitty-panel-ensure.
    ##
    local sock
    sock="$(kitty-panel-ensure)" || return $?

    kitty @ --to "${sock}" resize-os-window --match all --action=show >/dev/null || return $?

    local win
    win="$(kitty @ --to "${sock}" ls | command jq -r '[.[] | select(.wm_class == "kitty-panel") | .tabs[] | select(.is_active) | .windows[] | select(.is_active) | .id][0] // empty')"
    if test -n "${win}" ; then
        kitty @ --to "${sock}" focus-window --match "id:${win}" >/dev/null
    fi
}

function kitty-panel-hide {
    #: Hides every kitty OS window. A kitty that is not running is already
    #: hidden, so that is a success.
    ##
    local sock
    sock="$(kitty-socket-get 2>/dev/null)" || return 0

    kitty @ --to "${sock}" resize-os-window --match all --action=hide >/dev/null
}

function kitty-panel-recreate {
    #: Rebuilds the panel: a fresh panel OS window takes over every tab, in
    #: order, and the old one closes itself once empty. Needed when a setting
    #: that kitty applies only at creation has changed, such as
    #: `macos_ns_window_layer' in =configFiles/kitty/kitty.conf= (run
    #: `kitty @ load-config' first so kitty has the new value).
    #:
    #: Existing panels are renamed out of the way first, or kitty-panel-ensure
    #: would treat the old panel as the one to keep. `resize-os-window
    #: --action=os-panel' cannot change an OS window's class, so the rename is
    #: done through the tabs: they are moved into a temporary *normal* OS
    #: window, and kitty-panel-ensure then does what it always does with tabs
    #: outside the panel.
    ##
    local sock
    sock="$(kitty-socket-get)" || return $?

    local ls_json
    ls_json="$(kitty @ --to "${sock}" ls)" || return $?

    local -a panel_tabs
    panel_tabs=( ${(f)"$(command jq -r '.[] | select(.wm_class == "kitty-panel") | .tabs[].id' <<<"${ls_json}")"} )
    panel_tabs=( ${panel_tabs:#} )
    if (( ${#panel_tabs} == 0 )) ; then
        ecerr "$0: no panel to recreate"
        return 1
    fi

    #: A holding window, normal class. Its shell tab is closed once the real
    #: tabs have arrived.
    local holder_win
    holder_win="$(kitty @ --to "${sock}" launch --type=os-window --dont-take-focus)" || return $?
    local holder_tab
    holder_tab="$(kitty @ --to "${sock}" ls | command jq -r --argjson w "${holder_win}" '[.[] | .tabs[] | select(any(.windows[]; .id == $w)) | .id][0]')"

    local t
    for t in "${panel_tabs[@]}" ; do
        kitty @ --to "${sock}" detach-tab --match "id:${t}" --target-tab "id:${holder_tab}" >/dev/null ||
            ecerr "$0: could not move tab ${t} to the holding window"
    done
    kitty @ --to "${sock}" close-tab --match "id:${holder_tab}" >/dev/null

    #: The old panel is now empty and gone; this creates the new one and moves
    #: the tabs in from the holding window, which then closes itself too.
    kitty-panel-ensure >/dev/null
}
