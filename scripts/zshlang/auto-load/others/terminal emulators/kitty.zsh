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
    #: See docs/unix-sockets.md.
    ##
    local dir="${NIGHT_SOCKETS_DIR:-${HOME}/.local/state}"

    #: `(N)' so no match yields nothing rather than an error. Do NOT add `.':
    #: these are sockets, not regular files.
    local -a socks
    socks=( ${~${kitty_sockets_list_glob:-${dir}/kitty-*.sock}}(N) )
    (( ${#socks} )) || return 0

    local -a live
    live=( ${(f)"$(kitty-pids)"} )

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

function kitty-pids {
    #: The pid of every running kitty, one per line.
    #:
    #: `pgrep -x', never `-f': `-f' matches whole command lines, including our
    #: own. `-a' on macOS, because BSD pgrep leaves out its own ancestors unless
    #: told otherwise, and kitty is an ancestor of every shell it runs: without
    #: it, a shell in a kitty tab concludes that kitty is not running. procps
    #: pgrep leaves out only itself, and its `-a' prints command lines instead.
    ##
    if isDarwin ; then
        command pgrep -a -x kitty
    else
        command pgrep -x kitty
    fi
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
        live=( ${(f)"$(kitty-pids)"} )
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
#: * Hiding tabs
#: kitty can hide an OS window but not a tab, so hiding a tab detaches it into
#: an OS window of its own and hides that. See =docs/kitty-tab-hide.md=.
#:
#: A tab is named by its first window's id (`window_id:<id>' as a tab match),
#: because a tab id does not survive `detach-tab': kitty moves the windows into
#: a new tab. kitty reports no visibility, so a hidden tab is one that carries
#: the user variable `kitty_tab_hidden' and sits outside the main OS window.
#: The main OS window is the hyper+z panel (`wm_class' `kitty-panel'), or else
#: the first one holding an unmarked tab. A marked tab that something else put
#: back into the main window, such as the panel folding strays, counts as
#: shown.
typeset -g kitty_tab_jq_defs='
def kitty_tab_marked_p: any(.windows[]; .user_vars.kitty_tab_hidden != null);
def kitty_main_os:
  [ .[] | select(.wm_class == "kitty-panel") | .id ][0]
  // [ .[] | select(any(.tabs[]; kitty_tab_marked_p | not)) | .id ][0];
def kitty_tab_state($main; $os):
  if $os != $main and kitty_tab_marked_p then "hidden" else "shown" end;
'

function h-kitty-tabs-tsv {
    : "usage: h-kitty-tabs-tsv <socket> [<tab match>]; prints '<first window id> TAB shown|hidden TAB <title> TAB <window ids>' per tab, every tab by default"
    local sock="${1}" match="${2}"
    assert-args sock @RET
    ensure-cmd jq @RET

    local all matched
    all="$(kitty @ --to "${sock}" ls)" @TRET
    if test -n "${match}" ; then
        #: Fails with kitty's own "No matching tabs" when nothing matches.
        matched="$(kitty @ --to "${sock}" ls --match-tab "${match}")" @TRET
    else
        matched="${all}"
    fi

    #: Slurped rather than passed with `--argjson', which would put the whole
    #: listing on the command line.
    { ec "${all}" ; ec "${matched}" } | command jq -rs "${kitty_tab_jq_defs}"'
        .[0] as $all | [ .[1][].tabs[].id ] as $ids
        | ($all | kitty_main_os) as $main
        | $all[] | .id as $os
        | .tabs[] | select(.id as $t | any($ids[]; . == $t))
        | [ .windows[0].id, kitty_tab_state($main; $os), .title,
            ([ .windows[].id | tostring ] | join(",")) ]
        | @tsv'
}

function h-kitty-tab-main-tab {
    : "usage: h-kitty-tab-main-tab <socket>; sets REPLY to the id of the active tab in the main OS window"
    local sock="${1}"
    assert-args sock @RET

    REPLY="$(kitty @ --to "${sock}" ls | command jq -r "${kitty_tab_jq_defs}"'
        kitty_main_os as $main
        | [ .[] | select(.id == $main) | .tabs[] | select(.is_active) | .id ][0] // empty')" @TRET
    test -n "${REPLY}"
}

function h-kitty-tab-each {
    : "usage: h-kitty-tab-each <hide|show|toggle> <tab match>; applies the verb to every matching tab"
    local verb="${1}" match="${2}"
    assert-args verb match @RET

    local sock
    sock="$(kitty-socket-get)" @RET
    local rows
    rows="$(h-kitty-tabs-tsv "${sock}" "${match}")" @RET

    local row ret=0 wid state title wmatch action
    local -a f
    for row in ${(f)rows} ; do
        f=( "${(@ps:\t:)row}" )
        wid="${f[1]}" state="${f[2]}" title="${f[3]}"
        #: `set-user-vars' takes a window match, so name every window in it.
        wmatch="id:${(j: or id:)${(s:,:)f[4]}}"

        action="${verb}"
        if [[ "${action}" == toggle ]] ; then
            if [[ "${state}" == hidden ]] ; then
                action=show
            else
                action=hide
            fi
        fi

        if [[ "${action}" == hide ]] ; then
            if [[ "${state}" == hidden ]] ; then
                ecgray "kitty-tab-hide: ${title} is already hidden"
                continue
            fi
            #: The mark goes first: should the detach fail, a marked tab still
            #: in the main window counts as shown.
            {
                kitty @ --to "${sock}" set-user-vars --match "${wmatch}" kitty_tab_hidden=1 &&
                    kitty @ --to "${sock}" detach-tab --match "window_id:${wid}" &&
                    kitty @ --to "${sock}" resize-os-window --match "id:${wid}" --action hide &&
                    ecgray "kitty-tab-hide: ${title} is hidden; kitty-tab-show window_id:${wid} brings it back"
            } || ret=$?
        else
            if [[ "${state}" == shown ]] ; then
                ecgray "kitty-tab-show: ${title} is already shown"
                continue
            fi
            {
                if h-kitty-tab-main-tab "${sock}" ; then
                    #: kitty makes an arriving tab the active one, and closes
                    #: the emptied OS window by itself.
                    kitty @ --to "${sock}" detach-tab --match "window_id:${wid}" --target-tab "id:${REPLY}"
                else
                    #: Nothing to return to, only hidden windows: show its own.
                    kitty @ --to "${sock}" resize-os-window --match "id:${wid}" --action show
                fi &&
                    kitty @ --to "${sock}" set-user-vars --match "${wmatch}" kitty_tab_hidden &&
                    ecgray "kitty-tab-show: ${title} is back as the active tab"
            } || ret=$?
        fi
    done
    return "${ret}"
}

function kitty-tab-hide {
    : "usage: kitty-tab-hide <tab match>; takes the matching tabs out of the tab bar, each into a hidden OS window of its own, e.g. kitty-tab-hide title:htop"
    #: What runs in the tab keeps running. Measured: well under a second, and
    #: neither focus nor the main window's active tab moves.
    ##
    h-kitty-tab-each hide "$@"
}

function kitty-tab-show {
    : "usage: kitty-tab-show <tab match>; moves hidden tabs back into the main kitty window, where each becomes the active tab"
    #: The tab moves back rather than its window being shown: a shown window
    #: lands on whichever space macOS picks, which is what the panel exists to
    #: avoid. Nothing here focuses kitty, since focusing a hidden panel
    #: activates it on the wrong space; the tab is there at the next hyper+z.
    ##
    h-kitty-tab-each show "$@"
}

function kitty-tab-toggle {
    : "usage: kitty-tab-toggle <tab match>; hides the matching tabs that are shown, and shows those that are hidden"
    h-kitty-tab-each toggle "$@"
}

function kitty-tab-state {
    : "usage: kitty-tab-state <tab match>; sets REPLY to shown or hidden, failing unless exactly one tab matches"
    local match="${1}"
    assert-args match @RET

    local sock rows
    sock="$(kitty-socket-get)" @RET
    rows="$(h-kitty-tabs-tsv "${sock}" "${match}")" @RET

    local -a lines=( ${(f)rows} )
    if (( ${#lines} != 1 )) ; then
        ecerr "$0: ${#lines} tabs match ${match}, not one"
        return 1
    fi
    local -a f=( "${(@ps:\t:)lines[1]}" )
    REPLY="${f[2]}"
}

function kitty-tab-ls {
    : "usage: kitty-tab-ls [<tab match>]; lists kitty's tabs: the match that names each one, shown or hidden, and its title"
    local sock rows
    sock="$(kitty-socket-get)" @RET
    rows="$(h-kitty-tabs-tsv "${sock}" "${1}")" @RET

    local row
    local -a f
    for row in ${(f)rows} ; do
        f=( "${(@ps:\t:)row}" )
        ec "window_id:${f[1]}"$'\t'"${f[2]}"$'\t'"${f[3]}"
    done
}

function h-kitty-tab-fz {
    : "usage: h-kitty-tab-fz <verb> <shown|hidden|''> [query ...]; sets reply to the tab matches picked"
    #: `kitty_tab_fz_opts' reaches fz, e.g. `--no-multi' for a verb that only
    #: makes sense once.
    ##
    ensure-array kitty_tab_fz_opts
    local fz_opts=( "${kitty_tab_fz_opts[@]}" )

    local verb="${1}" state="${2}"
    shift 2
    local query
    query="$(fz-createquery "$@")"

    local rows
    rows="$(kitty-tab-ls)" @RET
    if test -n "${state}" ; then
        rows="${(F)${(@M)${(@f)rows}:#window_id:<->$'\t'${state}$'\t'*}}"
    fi
    if test -z "${rows}" ; then
        ecerr "$0: no ${state:+${state} }kitty tabs"
        return 1
    fi

    local picks
    picks="$(ec "${rows}" | fz --prompt="kitty-tab ${verb}> " --query "${query}" \
        --delimiter=$'\t' --with-nth=2.. "${fz_opts[@]}")" @RET

    reply=( ${${(f)picks}%%$'\t'*} )
    reply=( ${reply:#} )
    (( ${#reply} ))
}

function kitty-tab-hide-fz {
    : "usage: kitty-tab-hide-fz [query ...]; pick shown tabs and hide them"
    h-kitty-tab-fz hide shown "$@" @RET
    local -a picks=( "${reply[@]}" )

    local pick ret=0
    for pick in "${picks[@]}" ; do
        kitty-tab-hide "${pick}" || ret=$?
    done
    return "${ret}"
}

function kitty-tab-show-fz {
    : "usage: kitty-tab-show-fz [query ...]; pick a hidden tab and bring it back"
    #: One pick, since each shown tab becomes the active one.
    ##
    kitty_tab_fz_opts=( --no-multi ) h-kitty-tab-fz show hidden "$@" @RET

    kitty-tab-show "${reply[1]}"
}

function kitty-tab-toggle-fz {
    : "usage: kitty-tab-toggle-fz [query ...]; pick a tab and hide or show it"
    kitty_tab_fz_opts=( --no-multi ) h-kitty-tab-fz toggle '' "$@" @RET

    kitty-tab-toggle "${reply[1]}"
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
#: hyper+z in panel mode (the default, see `kitty_hotkey_mode' in
#: hammerspoon/core/window-media-bindings.lua): every tab of the one kitty
#: instance lives in a *panel* OS window that floats over fullscreen apps.
#: Creating, showing and hiding it is Lua now, in
#: hammerspoon/core/kitty-panel.lua, which talks to kitty over remote control
#: directly. It used to be kitty-panel-ensure / -show / -hide here, run in the
#: garden, so the key died whenever the garden did. Story:
#: ~/notes/public/subjects/tools/CLI/terminal emulators/Kitty/hotkey window.org
