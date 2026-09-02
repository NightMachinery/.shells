BTT_HS_NOISES_UID='5DBF0BE7-5822-459A-B450-36E3396124F9'
##
function h-hammerspoon-eval {
    : "[agfi:hammerspoon] -c <lua>, with its extension-loading chatter stripped

Hammerspoon interleaves lines like '-- Loading extension: task' into stdout the
first time an extension is used -- which is exactly when a watcher-spawned task
has just loaded one. Unfiltered, a result reads as
'true-- Loading extension: task' and a successful call is reported as a failure.

Only those lines and the outer whitespace go; everything else comes back
verbatim, so this is safe over multi-line results such as JSON. The exit status
is Hammerspoon's own -- and note that it is 0 whether or not the Lua found
anything, so callers must check the RESULT string, not the status."
    setopt localoptions extendedglob

    local out
    out="$(hammerspoon -c "$@")" || return $?

    local -a lines
    lines=( "${(@f)out}" )
    lines=( "${(@)lines:#-- *}" )
    out="${(pj:\n:)lines}"

    print -r -- "${${out##[[:space:]]##}%%[[:space:]]##}"
}
##
function hs-reload {
    : "reload the Hammerspoon config now, whether or not a hold is up"

    hammerspoon -c "hs.reload()"
}
aliasfn hammerspoon-reload hs-reload
alias hsr='hs-reload'
alias hhh='hs-reload'
##
#: Holding off the auto-reloader.
#:
#: Hammerspoon watches ~/scripts/hammerspoon/ and reloads the whole config the
#: moment a .lua changes. Mid-edit that loads a half-written module, and worse,
#: leaves the *old* code's canvases and timers behind - a state that reads like
#: a real bug and is not one. So take a hold while editing, and reload by hand
#: with [agfi:hs-reload] when you actually want to see your changes.
#:
#: One file per holder in ${hs_no_reload_dir}, its mtime the deadline. Several
#: agents edit this repo at once; a single flag would let whoever finished first
#: re-enable reloading under someone still typing, and a counter would stay
#: stuck forever the first time one of them was killed. See the "** Holds"
#: section of hammerspoon/core/reload.lua for the reading side.
typeset -g hs_no_reload_dir="${HOME}/.hs-no-reload"
#: Same default as the agent banner, and for the same reason: long enough to be
#: useful, short enough that forgetting it is not a lasting problem.
typeset -g hs_reload_hold_default="${hs_reload_hold_default:-30m}"

function h-hs-reload-holder {
    : "a name for whoever is asking, stable across calls, distinct between concurrent sessions"

    local id="${hs_reload_holder:-}"
    #: Exported into every shell Claude Code spawns (see [agfi:claude-code-p]),
    #: and unlike \$PPID it is the same in the shell that takes the hold and the
    #: one that releases it minutes later.
    test -n "$id" || id="${CLAUDE_CODE_SESSION_ID:-}"
    test -n "$id" || id="${TERM_SESSION_ID:-}"
    test -n "$id" || id=default

    #: It becomes a filename, so keep it to something that cannot escape the
    #: directory or need quoting.
    ec "${id//[^A-Za-z0-9_-]/-}"
}

function hs-reload-hold {
    : "hold off the auto-reloader for <dur>, default ${hs_reload_hold_default}

Renews rather than stacks: calling it again while you already hold one just
pushes your own deadline out."
    @darwinOnly

    local reason="${1:-editing}" dur="${2:-${hs_reload_hold_default}}"

    local secs
    secs="$(dur2sec "$dur")" @RET

    local holder
    holder="$(h-hs-reload-holder)" @RET

    #: strftime and EPOCHSECONDS rather than `date': GNU and BSD date disagree
    #: about how to add an offset, and both are on the PATH here. `touch -t'
    #: takes the same format either way, so it is safe unqualified.
    local until=$(( EPOCHSECONDS + secs ))

    mkdir -p "$hs_no_reload_dir" @TRET
    #: Contents are for humans only; nothing reads them to decide anything.
    local body
    body="reason: ${reason}"$'\n'"holder: ${holder}"$'\n'"until:  $(strftime '%Y-%m-%d %H:%M:%S' "$until")"
    ec "$body" > "${hs_no_reload_dir}/${holder}" @TRET
    #: A deadline in the mtime keeps the reading side to a single stat with no
    #: parsing, which matters because it runs on Hammerspoon's main thread.
    #: Written last, because writing the contents would otherwise reset it.
    touch -t "$(strftime '%Y%m%d%H%M.%S' "$until")" \
        "${hs_no_reload_dir}/${holder}" @TRET

    ecgray "$0: held for $(seconds-fmt-short "$secs") (${reason}); release with hs-reload-release"
}

function hs-reload-release {
    : "drop this session's hold; reloads if it was the last one"
    @darwinOnly

    local holder
    holder="$(h-hs-reload-holder)" @RET
    command rm -f "${hs_no_reload_dir}/${holder}"

    #: Only when nobody else is still editing. Reloading here is the point of
    #: releasing: everything changed while the hold was up is still unloaded.
    local remaining
    remaining="$(h-hs-reload-holds-live)"
    if test -n "$remaining" ; then
        ecgray "$0: released, but still held by: ${remaining}"
        return 0
    fi

    ecgray "$0: released, reloading"
    hs-reload
}

function h-hs-reload-holds-live {
    : "names of the holders whose deadline has not passed, one per line"

    test -d "$hs_no_reload_dir" || return 0

    #: zstat, not `stat': the binary is BSD on this machine and GNU on others,
    #: and they spell mtime differently. This is a zsh builtin, so neither.
    zmodload -F zsh/stat b:zstat 2>/dev/null

    local f
    for f in "${hs_no_reload_dir}"/*(N) ; do
        if [[ "$(zstat +mtime "$f")" -gt "$EPOCHSECONDS" ]] ; then
            ec "${f:t}"
        else
            #: Expired. Nothing depends on the cleanup - the reader ignores it
            #: either way - but leaving corpses around makes `hs-reload-holds'
            #: harder to read.
            command rm -f "$f"
        fi
    done
}

function hs-reload-holds {
    : "who is holding the auto-reloader, why, and for how much longer"
    @darwinOnly

    local live
    live="$(h-hs-reload-holds-live)"
    if test -z "$live" ; then
        ec "auto-reload: not held"
        return 0
    fi

    zmodload -F zsh/stat b:zstat 2>/dev/null

    local holder f left reason
    ec "$live" | while read -r holder ; do
        f="${hs_no_reload_dir}/${holder}"
        left=$(( $(zstat +mtime "$f") - EPOCHSECONDS ))
        reason="$(command grep -m1 '^reason: ' "$f" 2>/dev/null)"
        ec "auto-reload held by ${holder}, $(seconds-fmt-short "$left") left, ${reason#reason: }"
    done
}
##
function hs-popclickBttToggle() {
    local lis="$(serr hammerspoon -c 'popclickBttToggle()')"
    hs-popclick-btt-refresh
    @opts say '' @ hs-popclick2icon $lis

}
function hs-popclick-btt-refresh() {
    btt-refresh "$BTT_HS_NOISES_UID"
}
function hs-popclickBttGet() {
    local lis
    lis="$(serr hammerspoon -c 'popclickBttGet()')" || {
        ecerr "$0: could not get value from hammerspoon"
        return 1
        ##
        lis=''
    }
    hs-popclick2icon "$lis"
}
@opts-setprefix hs-popclickBttGet hs-popclick2icon
function hs-popclick2icon() {
    local lis="${1}" say="${hs_popclick2icon_say}"

    test -n "$say" && { pgrep -f HS_POPCLICK_HI | inargsf kill-withchildren }
    if [[ "$lis" == true ]] ; then
        ec "🎆"
        test -n "$say" && {
            awaysh-named HS_POPCLICK_HI hearinvisible "$GREENCASE_DIR/LittleMisfortune/23_06_MI_thatsmagicaldontyouthink.flac"
            # fsay "The magic flows"
        }
    elif [[ "$lis" == false ]] ; then
        ec "🌌"
        test -n "$say" && {
            awaysh-named HS_POPCLICK_HI hearinvisible "$GREENCASE_DIR/LittleMisfortune/09_23_MI_itdoesntlookthatmagical.flac"
            # fsay "Sealed forever"
        }
    else
        ec "🥶"
        test -n "$say" && {
            awaysh-named HS_POPCLICK_HI hearinvisible "$GREENCASE_DIR/LittleMisfortune/05_30_MI_yikesforever.flac"
            # fsay "Yikes forever"
        }
    fi
    true
}
##
function gradS-get() {
    hammerspoon -c gradS # locking done via garden sessions
}
##
# gradS=E
# function h_gradS-get() {
#     local lockStr="lock_gradS"
#     local lock="$(redism setnx $lockStr 5)"
#     if [[ "$lock" == 1 ]] ; then
#         gradS="$(hammerspoon -c gradS)"
#         silent redism del "$lockStr"
#     else
#         silent redism expire "$lockStr" 60 # for resilience
#     fi
# }
# function gradS-get() {
#     h_gradS-get
#     ec "$gradS"
# }
##
# function h_gradS-get() {
#     local lockStr="lock_gradS"
#     local lock="$(redism setnx $lockStr 5)"
#     if [[ "$lock" == 1 ]] ; then
#         deus @opts expire 0 od 0 @ eval-memoi hammerspoon -c gradS
#         silent redism del "$lockStr"
#     else
#         silent redism expire "$lockStr" 300 # for resilience
#         @opts expire 0 od 0 @ eval-memoi hammerspoon -c gradS
#     fi
# }
# function gradS-get() {
#     local out="$(h_gradS-get)"
#     if test -z "$out" ; then
#         out='empty'
#     fi
#     ec "$out"
# }
##
# function h_gradS-get() {
#     local lockStr="lock_gradS"
#     local lock="$(redism setnx $lockStr 5)"
#     if [[ "$lock" == 1 ]] ; then
#         hammerspoon -c gradS
#         silent redism del "$lockStr"
#         return 0
#     else
#         silent redism expire "$lockStr" 300 # for resilience
#         return 1
#     fi
# }
# function gradS-get() {
#     retry_sleep=0.1 serr retry h_gradS-get
# }
##
function hs-alert-v1 {
    @darwinOnly

    local msg="$*" dur="${alert_dur:-5}"

    msg="$(ecn $msg | text-wrap 90 | sdlit $'\n' '\n' | sdlit '"' '\"')" @TRET
    sout hammerspoon -c "hs.alert (\"$msg\", ${dur})" # outputs a UUID thingy
    # https://www.hammerspoon.org/docs/hs.alert.html
}
##
# The v2 engine lives in hammerspoon/alert/: coloured bands that
# stack instead of hiding each other, wrap instead of being cut off, and can
# flash the whole screen first.
#
# Knobs, all overridable per call:
#   alert_dur     seconds on screen (default 5)
#   alert_flash   fullscreen flash before settling, in seconds; 0 skips it
#   alert_fade    the flash fades in and out by default; 0/no for a hard cut,
#                 or a number of seconds for each ramp. The ramps fit inside
#                 alert_flash rather than lengthening it
#   alert_pos     top (default), center, bottom
#   alert_id      reusing an id updates that alert in place instead of stacking
#   alert_markup  plain (default) or md
#   alert_color   band colour by name. The originals are default, warn/amber,
#                 crit, agent, free and notice; there is also a palette
#                 (ocean, violet, blood, gold, forest, midnight, ...), three
#                 animated ones (rainbow-1, silver-pulse-1, wolf-eye-1), and
#                 any x11 colour name. An unknown name draws a default band.
#                 The text goes black or white to suit whichever it lands on.
#                 See hammerspoon/alert/colors.lua for the whole palette.
#
# `md` is a small markdown subset - **bold**, *italic*, ~~strike~~, and
# [text]{red bold} for colour, which markdown has none of. Anything that does
# not parse renders literally, so a typo shows up rather than vanishing. See
# hammerspoon/alert/markup.lua for the whole grammar.
#
# Both new knobs are bare words, so unlike the message they are safe to inline
# in the command string; the colour is resolved by name on the Lua side rather
# than sending an RGB table over ipc.
#
# The message travels in a file rather than in the command string. `hammerspoon
# -c` hangs on payloads of a few hundred characters and takes the ipc port down
# with it until the client is killed, so inlining the text - escaped or
# base64-encoded, it makes no difference - breaks on exactly the long command
# output this is most useful for. Only the path goes over ipc; the Lua side
# reads the file and deletes it.
#
# No text-wrap either: the engine wraps against actual pixels, so it knows the
# screen width and we do not have to guess at 90 columns.
function hs-alert-v2 {
    @darwinOnly

    local msg="$*" dur="${alert_dur:-5}"
    local flash="${alert_flash:-0.35}" pos="${alert_pos:-top}"
    local fade="${alert_fade:-}"
    local id="${alert_id:-}"
    local markup="${alert_markup:-plain}" color="${alert_color:-}"

    # Not `local path`: `path` is the array tied to $PATH, so declaring it local
    # and assigning a string to it empties PATH for the rest of the function.
    # Everything after that, `command mktemp` included, is a command-not-found.
    local msgfile
    msgfile="$(command mktemp "${TMPDIR:-/tmp}/hs-alert-v2.XXXXXX")" @TRET
    ecn "$msg" > "$msgfile" @TRET

    local opts="seconds = ${dur}, flashSeconds = ${flash}, position = \"${pos}\""
    opts+=", markup = \"${markup}\""
    if test -n "$id" ; then
        opts+=", id = \"${id}\""
    fi
    if test -n "$color" ; then
        opts+=", color = \"${color}\""
    fi
    #: Unset means the engine's own fade defaults, so say nothing at all. A bare
    #: number is passed through as the ramp length; anything else truthy just
    #: asks for the defaults explicitly. Never emit the word itself - an unquoted
    #: `yes` would reach Lua as an undefined global rather than as an error.
    if test -n "$fade" ; then
        if ! bool "$fade" ; then
            opts+=", floodFade = false"
        elif [[ "$fade" == (<->|<->.<->|.<->) ]] ; then
            opts+=", floodFade = ${fade}"
        else
            opts+=", floodFade = true"
        fi
    fi

    reval-dbg sout hammerspoon -c "alertV2FromFile(\"${msgfile}\", { ${opts} })"
}
aliasfn hs-alert hs-alert-v2
aliasfn alert hs-alert
#: Without these, `@opts dur 10 @ hs-alert msg` derives its prefix from the
#: command name and sets =hs_alert_dur=, which nothing reads.
#: [agfi:ensure-var-name] sanitises the dash away instead of erroring, so the
#: knob is silently ignored. `alert` happens to work by accident; pin all three.
@opts-setprefix hs-alert-v2 alert
@opts-setprefix hs-alert alert
@opts-setprefix alert alert
##
function hs-reval-alert {
    local alert_dur="${alert_dur:-1}"

    local out retcode=0
    out="$(reval "$@" 2>&1)" || retcode=$?

    if test -z "$out" ; then
        out="[No Output]"
    fi

    if (( retcode != 0 )) ; then
        out="[Error Code: $retcode]"$'\n\n'"${out}"
    fi

    out="> $(gq "$@")"$'\n\n'"${out}"

    hs-alert "$(ec "$out" | head -n 30)"
}
##
function hs-hyper-z() {
    hammerspoon -c 'eventtap.keyStroke(hyper, 6)'
}
function hs-hyper-x() {
    hammerspoon -c 'eventtap.keyStroke(hyper, 7)'
}
function hs-hyper-m() {
    hammerspoon -c 'eventtap.keyStroke(hyper, 46)'
}
function hs-cmd-v() {
    hammerspoon -c 'eventtap.keyStroke({"cmd"}, 9)'
}
##
function hs-type {
    local input
    input="$(in-or-args "$@")" @RET

    reval-ec hammerspoon -c "hs.eventtap.keyStrokes($(js-quote "$input"))"
}
##
function hs-focus-app {
    local app_name="$1"
    assert-args app_name @RET

    hammerspoon -c "focusApp('${app_name}')"
}

function focus-app {
    if isDarwin ; then
        hs-focus-app "$@"
    else
        @NA
    fi
}
##
