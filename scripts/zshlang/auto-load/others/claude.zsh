##
function claude {
    #: The shared preamble (editor, instruction sync, tab title, proxy) is
    #: [agfi:h-agent-launch]; what follows is Claude Code's own environment.
    ##
    #: Dynamic override: `claude_max_retries=30 claude ...` (or `claude-m`,
    #: `claude-work`, which call this function). Unattended children started
    #: by the tmux-subagents skill need a finite count so an API outage makes
    #: the process exit (the pane dies, the waiter fires) instead of retrying
    #: forever and looking alive.
    local -x CLAUDE_CODE_MAX_RETRIES="${claude_max_retries:-2147483647}"

    #: [[https://code.claude.com/docs/en/monitoring-usage][Monitoring - Claude Code Docs]]
    # Make stalled streaming connections fail/retry instead of hanging forever-ish.
    local -x CLAUDE_ENABLE_STREAM_WATCHDOG=1
    local -x CLAUDE_ENABLE_BYTE_WATCHDOG=1
    # local -x CLAUDE_STREAM_IDLE_TIMEOUT_MS=90000
    local -x CLAUDE_CODE_DISABLE_FEEDBACK_SURVEY=1

    # Debug logging. Use a stable per-run file so you can tail it.
    local debug_file="${HOME}/tmp/claude-code/${EPOCHSECONDS}.debug.log"
    local -x CLAUDE_CODE_DEBUG_LOG_LEVEL=verbose

    #: Claude Code rewrites the terminal title continuously (`✳ <summary>'),
    #: overwriting the marker [agfi:h-agent-launch] sets within seconds. This makes it leave the
    #: title alone -- at the cost of strategy 3 in
    #: [agfi:h-claude-code-session-of-kitty-window], which maps a kitty window
    #: to a session *by* that title. Strategies 1, 2 and 4 (foreground PID,
    #: tmux client, registry) are unaffected, and inside tmux it is strategy 2
    #: that fires, so this is cheap there and costly outside. Hence off by
    #: default. See =./docs/tmux-tty-title.md=.
    if bool "${claude_tty_title_keep_p}" ; then
        local -x CLAUDE_CODE_DISABLE_TERMINAL_TITLE=1
    fi

    #: Which seat this is, resolved once from the effective CLAUDE_CONFIG_DIR
    #: rather than from the launcher's name, so `claude-m` typed inside a work
    #: session is still a work session. Every visual cue below keys off it; the
    #: tables are generated from =configFiles/claude-code/profiles.yaml=.
    local profile
    profile="$(claude-code-profile-current)"

    #: The seat's own palette, as a =themes/profile.json= per config dir
    #: ([agfi:claude-themes-link]). The link must exist before the session
    #: starts: Claude Code only watches a themes directory that was already
    #: there. One stat in the common case, where the link is already right.
    local -a claude_args=()
    if bool "${claude_theme_p:-y}" ; then
        test -e "$(h-claude-profile-theme-link "${profile}")" ||
            claude-themes-link || true
    else
        #: `--settings` merges above the user settings for this session only.
        #: A caller that passes its own `--settings` may not merge with this
        #: one -- the tmux-subagents launcher is the case to watch -- which is
        #: why the default path passes none.
        claude_args+=(--settings '{"theme":"light-daltonized"}')
    fi

    #: Read by =configFiles/claude-code/statusline.sh=, which puts the seat's
    #: emoji first in the status line. The status line command inherits our
    #: environment, which is the only channel it has: its stdin JSON carries no
    #: profile field.
    local -x CLAUDE_CODE_PROFILE_BADGE_P="${claude_statusline_badge_p:-y}"

    #: A pale wash of the seat's colour behind the session: the one cue that
    #: needs no reading at all. Only when our stdout is a terminal, since a
    #: piped or captured run must not be handed escape codes, and only where
    #: the wash stays inside our own pane ([agfi:h-claude-tint-scope-p]).
    #: A seat with no entry in =claude_code_profile_tints= is not tinted, and
    #: is not reset on the way out either, so nothing is undone that was never
    #: done.
    local tint_p=n
    if bool "${claude_tint_p:-y}" && isTty && h-claude-tint-scope-p &&
        test -n "${claude_code_profile_tints[$profile]}" ; then
        tint_p=y
        h-claude-tint-set "${profile}"
    fi

    #: The seat's colour on the pane's own border: the same signal as the tint
    #: without repainting anything the session writes over. On by default, but
    #: only for a seat listed in =claude_code_profile_borders=, and only inside
    #: tmux, which is the only thing here that has a pane border to colour.
    local border_p=n border_had=''
    if bool "${claude_tmux_border_p:-y}" && isTmux &&
        test -n "${claude_code_profile_borders[$profile]}" ; then
        border_p=y
        border_had="$(command tmux show-options -pqv -t "${TMUX_PANE}" pane-border-style 2>/dev/null)"
        h-claude-tmux-border-set "${profile}"
    fi

    #: On by default, since the border above already costs the window its
    #: border row: once that row exists it may as well name the seat instead of
    #: showing tmux's default `0 "<pane title>"'. A pane already labelled keeps
    #: its label afterwards, the value having been somebody else's to begin
    #: with, so only a label we introduced is taken away again.
    local label_p=n label_had=''
    if bool "${claude_tmux_label_p:-y}" && isTmux ; then
        label_p=y
        label_had="$(command tmux show-options -pqv -t "${TMUX_PANE}" "${claude_tmux_label_option}" 2>/dev/null)"
        h-claude-tmux-label-set "${profile}"
    fi

    {
        #: The marker is how a work tab is told apart from a personal one, in
        #: the tty title and in every other cue. `local` is dynamically scoped
        #: in zsh, so a caller can override it without exporting anything.
        agent_launch_glyph="${claude_tty_title_marker:-${claude_code_profile_markers[$profile]:-$(h-agent-field claude glyph)}}" \
            h-agent-launch claude command claude "${claude_args[@]}" "$@"
    } always {
        #: Runs on a normal quit and on Ctrl-C alike; only a `kill -9` escapes
        #: it. Never allowed to change the session's own exit status.
        if bool "${tint_p}" ; then
            h-claude-tint-reset || true
        fi
        if bool "${label_p}" && test -z "${label_had}" ; then
            #: The pane's own label first: the row is only taken away once no
            #: pane in the window still needs it, and this pane no longer does.
            h-claude-tmux-label-unset || true
            h-claude-tmux-label-row-restore || true
        fi
        if bool "${border_p}" && test -z "${border_had}" ; then
            h-claude-tmux-border-unset || true
        fi
    }
}
aliasfn claude-m claude
##
function claude-install-npm {
    #: npm runs postinstall scripts unconditionally, so there is no
    #: --allow-build equivalent to pass here.
    ##
    h-npm-install-clean-staging '@anthropic-ai/claude-code' @RET

    reval-ecgray npm-install-npm '@anthropic-ai/claude-code@latest' @RET
    h-npm-install-report claude
}

function claude-install-pnpm {
    #: Currently broken, the same way [agfi:codex-install-pnpm] is:
    #: claude-code's darwin-arm64 tarball is ~83MB, over the size at which
    #: pnpm's worker-thread integrity check aborts the process. See
    #: [agfi:npm-install]. Kept so the pnpm route stays one word away once
    #: pnpm or node fixes it.
    #:
    #: pnpm 10 does not run a dependency's postinstall script unless the
    #: package is named in --allow-build, and claude-code's postinstall
    #: (`install.cjs') is what puts the native binary in place. Without it the
    #: install "succeeds" and leaves you with no working `claude'.
    #:
    #: `local' is dynamically scoped in zsh, so [agfi:npm-install] picks this
    #: up without it being exported.
    ##
    local npm_install_pnpm_opts=(--allow-build='@anthropic-ai/claude-code')

    reval-ecgray npm-install-pnpm '@anthropic-ai/claude-code@latest' @RET
    h-npm-install-report claude
}

function claude-install {
    #: npm for now, because pnpm cannot install claude-code at all; see
    #: [agfi:claude-install-pnpm].
    ##
    claude-install-npm "$@"
}
##
function claude-autocommit {
    local -x ANTHROPIC_MODEL="sonnet"

     reval-ec claude-m --verbose -p 'git-committer' --allowedTools 'Bash(git:*)'

     ecgray
     reval-ecgray glola 5
}
##
function claude-vcsh-commit {
    local target_dir="${1:-$NIGHTDIR}"
    local engine=("${claude_commit_engine[@]:-claude-m}")

    (
        local -x ANTHROPIC_MODEL="sonnet"

        cd "$target_dir" @RET
    
        reval-ecgray "${engine[@]}" -p "Read '${NIGHTDIR}/AGENTS.md' and '${NIGHTDIR}/PE/vcsh-commit.md' and start committing changes." --verbose --allowedTools 'Bash(vcsh night.sh:*)'

        ecgray
        reval-ecgray vcn-with glola 5
    )
}

function claude-night-sh {
    (
        cd "$NIGHTDIR" @RET
        
        claude-m "${NIGHTDIR}/prompt/night-sh.md"
    )
}
##
function claude-pioneer {
    local -x ANTHROPIC_AUTH_TOKEN="${pioneer_api_key}"
    local -x ANTHROPIC_BASE_URL="https://api.pioneer.ai/"

    claude "$@"
}

# aliasfn claude-m claude-pioneer
##
function claude-freemodel {
    local -x ANTHROPIC_AUTH_TOKEN="${freemodel_api_key}"
    local -x ANTHROPIC_API_KEY="${freemodel_api_key}"
    local -x ANTHROPIC_BASE_URL="https://cc.freemodel.dev"
    local -x CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC=1

    claude "$@"
}
##
function claude-highwayai {
    local -x ANTHROPIC_AUTH_TOKEN="NA"
    local -x ANTHROPIC_API_KEY="NA"
    local -x ANTHROPIC_BASE_URL="https://freeapi.highwayapi.ai/anthropic"
    local -x CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC=1

    claude "$@"
}
##
#: The seats themselves -- their config dirs, launchers, markers, labels,
#: colours, tints, borders and themes -- are not defined here. They come from
#: =configFiles/claude-code/profiles.yaml= via
#: =zshlang/auto-load/others/claude-profiles.gen.zsh=, which this file sources
#: at startup like any other auto-load file:
#:
#:   claude_code_profiles            each seat's =CLAUDE_CONFIG_DIR=, or empty
#:   claude_code_profile_order       iteration and display order
#:   claude_code_profile_homes       the basename of each config home
#:   claude_code_profile_launchers   the command that starts a seat
#:   claude_code_profile_markers     one emoji per seat
#:   claude_code_profile_labels      the word a human reads
#:   claude_code_profile_colors      an `R;G;B' identity colour
#:   claude_code_profile_tints       the pane background wash
#:   claude_code_profile_borders     the tmux border attributes
#:   claude_code_profile_themes      which tracked theme file to link
#:
#: One YAML rather than three hand-kept copies, because the same seats have to
#: be known to this launcher, to the Go session pickers
#: (=internal/profiles=) and to the bash status line, and they had already
#: drifted once. Nothing parses the YAML at runtime -- a parse costs 30ms and
#: this file is sourced by every interactive shell -- so edit the YAML and run
#: [agfi:agent-profiles-sync]; `go test ./internal/profiles/' fails if any
#: generated copy is stale.
#:
#: The convention throughout is that a seat with no entry gets no cue: the
#: default seat is the baseline the terminal is already set up for, and is
#: recognised by being left alone. Every cue is derived from the *effective*
#: config dir ([agfi:claude-code-profile-current]), never from the launcher's
#: name, so `claude-m' typed inside a work session is still work. See
#: =docs/claude_code_usage.md=.
##

function agent-profiles-sync {
    : "regenerates the per-language seat tables from profiles.yaml"
    #: The zsh tables, the Go module's =internal/profiles= and the status
    #: line's JSON all come from one YAML. Run this after editing it; the
    #: launchers do not, because a stale copy is caught by
    #: `go test ./internal/profiles/' and re-generating on every launch would
    #: put python on the startup path.
    ##
    ensure-cmd python3 @RET
    revaldbg python3 "${NIGHTDIR}/python/agent_profiles_gen.py" "$@"
}
aliasfn agent-profiles-check agent-profiles-sync --check

function claude-themes-link {
    : "points each profile's themes/profile.json at its tracked theme file"
    #: Claude Code reads custom themes from =<config dir>/themes/<slug>.json=
    #: and watches that directory afterwards -- but only if it existed when the
    #: session started, which is why [agfi:claude] links before launching
    #: rather than after. Idempotent, and silent about profiles that are not
    #: installed on this host.
    ##
    local profile dir src
    for profile in "${claude_code_profile_order[@]}" ; do
        #: The default profile has no CLAUDE_CONFIG_DIR; its config home is
        #: =~/.claude=.
        dir="${claude_code_profiles[$profile]:-${HOME}/.claude}"
        test -d "${dir}" || continue

        src="${NIGHTDIR}/configFiles/claude-code/themes/${claude_code_profile_themes[$profile]}.json"
        if ! test -e "${src}" ; then
            ecerr "$0: missing tracked theme: ${src}"
            return 1
        fi

        mkdir -p "${dir}/themes" @RET
        command ln -sf "${src}" "${dir}/themes/profile.json" @RET
    done
}

#: The tmux pane option [agfi:h-claude-tmux-label-set] writes and the
#: pane-border format reads. A pane option, so it describes one pane and not
#: the session, which may hold several seats at once.
typeset -g claude_tmux_label_option='@claude_profile'
#: Set on the *window* when [agfi:h-claude-tmux-label-set] turns the border row
#: on, so [agfi:h-claude-tmux-label-row-restore] can tell a row we introduced
#: from one the window already had.
typeset -g claude_tmux_label_row_option='@claude_border_row'

function h-claude-tmux-label-set {
    : "labels this tmux pane with the profile and shows the border row"
    #: Unlike the other cues this one is off by default
    #: (`claude_tmux_label_p=y' to ask for it): the border row costs a line of
    #: every pane in the window, which is a poor trade when the theme and the
    #: tint already say the same thing.
    ##
    local profile="${1}"
    assert-args profile @RET
    test -n "${TMUX_PANE}" || return 0

    local marker="${claude_code_profile_markers[$profile]}"
    local label="${marker:+${marker} }${claude_code_profile_labels[$profile]:-${(U)profile}}"

    command tmux set-option -p -t "${TMUX_PANE}" \
        "${claude_tmux_label_option}" "${label}" 2>/dev/null || return 0

    #: The border row and its format are *window* properties, so they are only
    #: set when the window has no format of its own to lose, and are left in
    #: place afterwards: clearing them would blank the label of any other
    #: labelled pane in the same window. Panes without the option read `SHELL'.
    local existing
    existing="$(command tmux show-options -wqv -t "${TMUX_PANE}" pane-border-format 2>/dev/null)"
    test -n "${existing}" && return 0

    #: The pane index is shown only in a split window. tmux's own format leads
    #: with it, which in the usual one-pane window is a bare `0' in front of
    #: the name and tells the reader nothing.
    command tmux set-option -w -t "${TMUX_PANE}" pane-border-status top 2>/dev/null
    command tmux set-option -w -t "${TMUX_PANE}" pane-border-format \
        "#[bold] #{?#{${claude_tmux_label_option}},#{${claude_tmux_label_option}},SHELL} #[default]#{?#{>:#{window_panes},1},#{pane_index} ,}" 2>/dev/null

    #: Remembered on the window, not in a shell variable: whoever takes the row
    #: away again may be a different shell entirely ([agfi:agent-done] runs
    #: from the agent's tool shell, not from this launcher).
    command tmux set-option -w -t "${TMUX_PANE}" "${claude_tmux_label_row_option}" 1 2>/dev/null
}

function h-claude-tmux-label-row-restore {
    : "hides the pane border row again, if we are the ones who showed it"
    #: The row and its format are *window* options, so they outlive the pane
    #: the session ran in: without this, a finished session leaves the window
    #: with a border line that has nothing left to say and reads `SHELL'.
    #:
    #: Only removed when we turned it on -- recorded by
    #: [agfi:h-claude-tmux-label-set] -- and when no other pane in the window
    #: still carries a label, since every labelled pane in the window is drawn
    #: by this one format. Unset rather than set to `off', so the window goes
    #: back to whatever the session or the global default says.
    ##
    test -n "${TMUX_PANE}" || return 0

    local ours
    ours="$(command tmux show-options -wqv -t "${TMUX_PANE}" "${claude_tmux_label_row_option}" 2>/dev/null)"
    test -n "${ours}" || return 0

    local pane label
    for pane in ${(f)"$(command tmux list-panes -F '#{pane_id}' -t "${TMUX_PANE}" 2>/dev/null)"} ; do
        [[ "${pane}" == "${TMUX_PANE}" ]] && continue
        label="$(command tmux show-options -pqv -t "${pane}" "${claude_tmux_label_option}" 2>/dev/null)"
        #: Another session is still using the row; leave it and its format be.
        test -n "${label}" && return 0
    done

    command tmux set-option -wu -t "${TMUX_PANE}" pane-border-format 2>/dev/null || true
    command tmux set-option -wu -t "${TMUX_PANE}" pane-border-status 2>/dev/null || true
    command tmux set-option -wu -t "${TMUX_PANE}" "${claude_tmux_label_row_option}" 2>/dev/null || true
}

function h-claude-tmux-cues-teardown {
    : "undoes every cue [agfi:claude] painted, for a session that is being killed"
    #: [agfi:agent-done] calls this instead of relying on the launcher's
    #: `always' block, which a killed pane never reaches. Unconditional on
    #: purpose: the pane is about to go, so restoring a pane option somebody
    #: else set is moot, while leaving the window's border row behind is not.
    ##
    h-claude-tmux-label-unset || true
    h-claude-tmux-border-unset || true
    h-claude-tmux-label-row-restore || true
    h-claude-tint-reset || true
}

function h-claude-tmux-label-unset {
    : "removes the pane label [agfi:h-claude-tmux-label-set] wrote"
    test -n "${TMUX_PANE}" || return 0

    command tmux set-option -pu -t "${TMUX_PANE}" \
        "${claude_tmux_label_option}" 2>/dev/null || true
}

function h-claude-profile-color-hex {
    : "prints a profile's colour as #rrggbb, from its R;G;B triplet"
    #: tmux styles want a hex colour or a palette index, while the theme files
    #: and the Go pickers want the triplet. Deriving one from the other keeps
    #: the colour itself written down exactly once.
    ##
    local profile="${1}"
    assert-args profile @RET

    local rgb="${claude_code_profile_colors[$profile]}"
    test -n "${rgb}" || return 1

    local -a c
    c=("${(@s.;.)rgb}")
    (( ${#c} == 3 )) || return 1

    printf '#%02x%02x%02x\n' "${c[1]}" "${c[2]}" "${c[3]}"
}

#: Both halves of a pane's border, since tmux picks between them by whether the
#: pane is the active one and we want the seat's colour either way.
typeset -ga claude_tmux_border_options=(pane-border-style pane-active-border-style)

function h-claude-tmux-border-set {
    : "colours this pane's borders in the seat's colour; see =claude_code_profile_borders="
    local profile="${1}"
    assert-args profile @RET
    test -n "${TMUX_PANE}" || return 0

    local attrs="${claude_code_profile_borders[$profile]}"
    test -n "${attrs}" || return 0

    local hex
    hex="$(h-claude-profile-color-hex "${profile}")" || return 0

    local opt
    for opt in "${claude_tmux_border_options[@]}" ; do
        command tmux set-option -p -t "${TMUX_PANE}" "${opt}" "fg=${hex},${attrs}" 2>/dev/null
    done

    #: A window with one pane draws no borders at all, so without this the cue
    #: would be invisible in exactly the common case: one agent, one pane, one
    #: window. Turning the border row on costs that window a line, which is
    #: the whole price of this cue. Left on afterwards, like the label's window
    #: options, because clearing it would blank the border of any other pane
    #: that is relying on it.
    local status_now
    status_now="$(command tmux show-options -wqv -t "${TMUX_PANE}" pane-border-status 2>/dev/null)"
    if [[ "${status_now}" == (''|off) ]] ; then
        command tmux set-option -w -t "${TMUX_PANE}" pane-border-status top 2>/dev/null
    fi
}

function h-claude-tmux-border-unset {
    : "drops the per-pane border styles [agfi:h-claude-tmux-border-set] set"
    test -n "${TMUX_PANE}" || return 0

    local opt
    for opt in "${claude_tmux_border_options[@]}" ; do
        command tmux set-option -pu -t "${TMUX_PANE}" "${opt}" 2>/dev/null || true
    done
}

function h-claude-tint-scope-p {
    : "true when the tint applies in this context; see =claude_tint_scope="
    #: OSC 11 reaches only our own pane under tmux, but a bare terminal has no
    #: panes, so outside tmux the same escape recolours the entire window --
    #: the user's terminal, not just the session's corner of it. That is too
    #: much to take by default, so `tmux' is the default scope and `always'
    #: opts into the window-wide version.
    ##
    local scope="${claude_tint_scope:-tmux}"

    case "${scope}" in
        always) return 0 ;;
        tmux) isTmux ;;
        never) return 1 ;;
        *)
            ecerr "$0: unknown claude_tint_scope: ${scope} (tmux, always, never)"
            return 1
            ;;
    esac
}

function h-claude-tint-set {
    : "tints this pane (under tmux) or window (bare) to a profile's wash"
    #: OSC 11 sets the default background colour. tmux 3.0 and later apply it
    #: to the pane that sent it, so a sibling pane in the same window is
    #: untouched; outside tmux, kitty applies it to the whole window. Cells the
    #: TUI paints with a background of their own are unaffected, so this reads
    #: as a tint rather than a repaint.
    ##
    local profile="${1}"
    assert-args profile @RET

    local tint="${claude_code_profile_tints[$profile]}"
    test -n "${tint}" || return 0

    color-background "${tint}"
}

function h-claude-tint-reset {
    : "restores the terminal's own background, undoing [agfi:h-claude-tint-set]"
    #: A `kill -9` of the session skips this, leaving the pane tinted until
    #: something else resets it or a new pane replaces it.
    ##
    color-background-reset
}

function h-claude-profile-theme-link {
    : "prints the path of a profile's themes/profile.json"
    local profile="${1}"
    assert-args profile @RET

    ec "${claude_code_profiles[$profile]:-${HOME}/.claude}/themes/profile.json"
}

function claude-code-profile-current {
    #: Prints the profile the Claude Code that spawned this shell runs under,
    #: by matching the CLAUDE_CONFIG_DIR it exports against
    #: =claude_code_profiles=. Unset means =default=. An unregistered config
    #: dir is named after its directory, so a third profile still gets a
    #: readable answer.
    ##
    local dir="${CLAUDE_CONFIG_DIR%/}"

    local p
    for p in "${claude_code_profile_order[@]}" ; do
        if [[ "${claude_code_profiles[$p]%/}" == "${dir}" ]] ; then
            ec "${p}"
            return 0
        fi
    done

    ec "${${dir:t}#.}"
}

function h-claude-code-profile-assert {
    local profile="${1}"
    assert-args profile @RET

    if (( ${+claude_code_profiles[$profile]} == 0 )) ; then
        #: Listed from the ordered array, not the assoc, whose key order is
        #: arbitrary.
        ectrace "$0: unknown profile: ${profile} (known: ${(j:, :)claude_code_profile_order})"
        return 1
    fi
}

function h-claude-code-usage-argv-common {
    #: The flags that do not depend on which profile is being reported.
    #: Shared by [agfi:claude-code-usage] and [agfi:claude-code-usage-all], so
    #: the two cannot drift apart.
    ##
    local timeout_s="${claude_code_usage_timeout_s:-10}"
    local cache_ttl_s="${claude_code_usage_cache_ttl_s:-300}"
    local refresh_p="${claude_code_usage_refresh_p:-n}"
    local json_p="${claude_code_usage_json_p:-n}"
    local strip_ansi_p="${claude_code_usage_strip_ansi_p:-n}"

    local args=(
        --timeout "${timeout_s}"
        --cache-ttl "${cache_ttl_s}"
    )
    if bool "${refresh_p}" ; then
        args+=(--refresh)
    fi
    if bool "${json_p}" ; then
        args+=(--json)
    fi
    if bool "${strip_ansi_p}" ; then
        args+=(--color never)
    fi

    ec "${(F)args}"
}

function claude-code-usage {
    #: Shows the usage stats of one Claude Code profile's plan (like the in-app
    #: =/usage=). [agfi:claude-code-usage-all] does every registered profile at
    #: once, and is what the bare =ccu=/=ccs= aliases run.
    #: See =docs/claude_code_usage.md=.
    ##
    local profile="${claude_code_usage_profile:-default}"
    local notif_p="${claude_code_usage_notif_p:-n}"

    ensure-cmd claude_code_usage.py @RET
    h-claude-code-profile-assert "${profile}" @RET

    local common
    common=("${(@f)$(h-claude-code-usage-argv-common)}") @RET

    local script_args=(
        --profile-label "${profile}"
        --config-dir "${claude_code_profiles[$profile]}"
        "${common[@]}"
    )

    #: =script_args= before user args so explicit CLI flags win (argparse last-wins).
    local retcode=0
    $proxyenv revaldbg command claude_code_usage.py "${script_args[@]}" "$@" || retcode=$?

    if (( retcode == 0 )) && bool "${notif_p}" ; then
        #: After the report, so the human output is not held up and the
        #: notifier reads the cache this call has just written.
        #:
        #: =>&2= because our stdout may be a JSON document that a caller is
        #: about to parse; and never fatal, since a failed arm must not make a
        #: working usage report look broken.
        h-claude-code-usage-notif-for-profile "${profile}" >&2 || true
    fi

    return "${retcode}"
}

#: Arming is off by default -- checking usage should not be the same act as
#: asking to be told about it -- so each report has a =-notify= twin that turns
#: it on. Do not confuse these with the =-notif= functions further down, which
#: only arm and print no report.
aliasfnq claude-code-usage-notify claude_code_usage_notif_p=y claude-code-usage
aliasfn ccun claude-code-usage-notify

#: An explicit name for the profile [agfi:claude-code-usage] already reports by
#: default, so a caller never has to rely on that default being what they think.
function claude-code-usage-default {
    claude_code_usage_profile=default claude-code-usage "$@"
}
aliasfn claude-code-status-default claude-code-usage-default
alias ccu-default='claude-code-usage-default'
alias ccs-default='claude-code-usage-default'

aliasfnq claude-code-usage-default-notify claude_code_usage_notif_p=y claude-code-usage-default
alias ccu-default-notify='claude-code-usage-default-notify'
alias ccs-default-notify='claude-code-usage-default-notify'

function claude-code-usage-work {
    claude_code_usage_profile=work claude-code-usage "$@"
}
aliasfn claude-code-status-work claude-code-usage-work
alias ccu-work='claude-code-usage-work'
alias ccs-work='claude-code-usage-work'

aliasfnq claude-code-usage-work-notify claude_code_usage_notif_p=y claude-code-usage-work
alias ccu-work-notify='claude-code-usage-work-notify'
alias ccs-work-notify='claude-code-usage-work-notify'

function claude-code-usage-all {
    #: Every registered profile: separate accounts and separate requests, so
    #: there is nothing to serialize.
    #:
    #: The fan-out lives in the Python, one process running threads over what
    #: is pure network wait, the same way =codex_status.py= checks several auth
    #: files. Fanning out in the shell instead would mean a process per profile
    #: whose stdout is a pipe rather than the terminal, and =--color auto= would
    #: then quietly resolve to "no colour" for the command run most often.
    ##
    local profiles=("${claude_code_profile_order[@]}")
    assert-args profiles @RET

    local notif_p="${claude_code_usage_notif_p:-n}"

    ensure-cmd claude_code_usage.py @RET

    local common
    common=("${(@f)$(h-claude-code-usage-argv-common)}") @RET

    local script_args=(--all "${common[@]}")
    local p
    for p in "${profiles[@]}" ; do
        h-claude-code-profile-assert "${p}" @RET

        script_args+=(--profile "${p}=${claude_code_profiles[$p]}")
    done

    local retcode=0
    $proxyenv revaldbg command claude_code_usage.py "${script_args[@]}" "$@" || retcode=$?

    if bool "${notif_p}" ; then
        #: Not gated on the exit status, unlike the single-profile case: with
        #: several profiles a nonzero status only means *one* of them failed,
        #: and the rest still deserve their notifier.
        #:
        #: =>&2= because our stdout may be a JSON document a caller is about to
        #: parse; non-fatal because a failed arm must not make a working report
        #: look broken.
        for p in "${profiles[@]}" ; do
            h-claude-code-usage-notif-for-profile "${p}" >&2 || true
        done
    fi

    return "${retcode}"
}
aliasfn claude-code-status claude-code-usage-all
alias ccu='claude-code-usage-all'
alias ccs='claude-code-usage-all'

aliasfnq claude-code-usage-all-notify claude_code_usage_notif_p=y claude-code-usage-all
#: The bare short names mean every profile, so their =-notify= twins do too.
aliasfn claude-code-status-notify claude-code-usage-all-notify
alias ccu-notify='claude-code-usage-all-notify'
alias ccs-notify='claude-code-usage-all-notify'
##
#: How often the armed job re-checks the wall clock.
typeset -g claude_code_usage_notif_poll_s="${claude_code_usage_notif_poll_s:-30}"
#: Fire this many seconds after the reset, so the endpoint has actually flipped
#: by the time we claim it has.
typeset -g claude_code_usage_notif_grace_s="${claude_code_usage_notif_grace_s:-30}"
#: Utilization at or above which a window counts as blocking us.
typeset -g claude_code_usage_notif_full_pct="${claude_code_usage_notif_full_pct:-100}"
#: What the armed job does once the limits reset: =notif= to tell you, or
#: =type-continue= to type into the session that was blocked.
typeset -g claude_code_usage_notif_action="${claude_code_usage_notif_action:-notif}"
#: Only resume when the keyboard has been untouched at least this long. If you
#: are at the machine you get a notification instead and can resume yourself.
typeset -g claude_code_usage_type_continue_idle_min_s="${claude_code_usage_type_continue_idle_min_s:-600}"
#: Typing waits longer after a reset than a notification does: an early
#: notification is harmless, an early resume is spent on a session that is
#: still blocked.
typeset -g claude_code_usage_type_continue_grace_s="${claude_code_usage_type_continue_grace_s:-60}"
#: What gets typed. A carriage return is appended to submit it.
typeset -g claude_code_usage_type_continue_text="${claude_code_usage_type_continue_text:-Continue.}"

function h-claude-code-usage-notif-window {
    #: Prints "<percent>\t<resets_at_epoch>\t<label>" for one window of a
    #: =claude-code-usage --json= payload, and fails when that window is
    #: absent -- a team seat, for one, has no weekly window at all.
    #:
    #: Roles: =session=, =weekly_all=, =weekly:<ModelDisplayName>=.
    ##
    local json="${1}" role="${2}"
    assert-args json role @RET

    ensure-cmd jq @RET

    #: Matching the normalized =.windows[]= on =key= covers both payload
    #: shapes: the authoritative =limits[]= array (session, weekly_all,
    #: weekly_scoped) and the legacy objects (five_hour, seven_day,
    #: seven_day_*).
    local filter='' model=''
    case "${role}" in
        session)
            filter='.key == "session" or .key == "five_hour"'
            ;;
        weekly_all)
            filter='.key == "weekly_all" or .key == "seven_day"'
            ;;
        weekly:*)
            #: Model-scoped weekly windows all share the key "weekly_scoped",
            #: so the model itself only survives in the label ("7d Fable").
            #: =contains=, not =test=, so a model name is never read as a
            #: regex; and passed via =--arg=, so it cannot break out of the
            #: jq program either.
            model="${${role#weekly:}:l}"
            assert-args model @RET

            filter='(.key == "weekly_scoped" or (.key | startswith("seven_day_"))) and (.label | ascii_downcase | contains($model))'
            ;;
        *)
            ectrace "$0: unknown role: ${role}"
            return 1
            ;;
    esac

    ec "${json}" |
        jq -er --arg model "${model}" "[.windows[] | select(${filter})] | first
            | select(. != null)
            | [(.utilization_percent // 0), (.resets_at // 0), .label]
            | @tsv"
}

function h-claude-code-usage-notif-session {
    #: The tmux session a profile's notifier lives in, named after the
    #: scheduling function minus the =h-= so that `tmux ls` and the function
    #: you called line up. Uniform across profiles, the default one included.
    local profile="${1}"
    assert-args profile @RET

    ec "claude-code-usage-${profile}-notif-schedule"
}

function h-claude-code-usage-notif-wait {
    #: The armed one-shot body, running inside the tmux session that
    #: [agfi:h-claude-code-usage-notif] creates. This has to be a function: a
    #: bare =sleep= does not keep the marked subshell alive (see =PE/Zsh.org=).
    ##
    local poll_s="${claude_code_usage_notif_poll_s:-30}"

    local deadline="${1}" profile="${2}" msg="${3}"
    assert-args deadline profile msg @RET

    zmodload zsh/datetime 2>/dev/null

    #: Poll the wall clock rather than issuing one long =sleep=: a suspend
    #: would skew a single five-hour sleep, and on wake we want to fire
    #: straight away instead of however long the machine slept later.
    while (( EPOCHSECONDS < deadline )) ; do
        sleep "${poll_s}"
    done

    h-claude-code-usage-notif-fire "${profile}" "${msg}"
}

function h-claude-code-usage-notif-notify {
    #: A stable group, so a repeat replaces the previous notification instead
    #: of stacking up in Notification Center. See =docs/bell-auto.md=.
    ##
    local msg="${1}"
    assert-args msg @RET

    notif_group='claude-code-usage' notif "${msg}"
}

function h-claude-code-usage-notif-log {
    #: One line per fire. The tmux pane a fired job leaves behind says the same
    #: thing, but only until the next reboot, and a job that types into your
    #: session while you are away should stay answerable for it afterwards.
    ##
    local msg="${1}"
    assert-args msg @RET

    zmodload zsh/datetime 2>/dev/null

    local log="${claude_code_usage_notif_log:-${HOME}/logs/claude-code-usage-notif.log}"
    ensure-dir "${log:h}" || return 0

    print -r -- "$(strftime '%Y-%m-%d %H:%M:%S' "${EPOCHSECONDS}") ${msg}" >> "${log}"
}

function h-claude-code-usage-idle-s {
    #: How long the keyboard and mouse have been untouched, in whole seconds.
    #:
    #: Through [agfi:h-hammerspoon-eval], which strips the extension-loading
    #: chatter that would otherwise turn a result into
    #: `0-- Loading extension: host`. Hammerspoon exits 0 whether or not the Lua
    #: found anything, so what gets checked is the RESULT: a non-number means
    #: "cannot tell", and the caller declines to type on that.
    ##
    local out
    out="$(h-hammerspoon-eval 'return hs.host.idleTime()')" || return 1

    [[ "${out}" =~ '^[0-9]+(\.[0-9]+)?$' ]] || return 1

    ec "${out%%.*}"
}

function h-claude-code-usage-screen-locked-p {
    #: True when the screen is locked. The key is *absent* rather than false
    #: when unlocked, so the Lua compares and we test the resulting string.
    ##
    local out
    out="$(h-hammerspoon-eval 'return tostring(hs.caffeinate.sessionProperties()["CGSSessionScreenIsLocked"] == true)')" || return 1

    [[ "${out}" == true ]]
}

function h-claude-code-usage-type-continue-send {
    #: Delivers the resume text to one target: `kitty:<window-id>` types into
    #: that window, `codex:<thread-id>` queues the message with Codex itself,
    #: and `frontmost` types wherever the keyboard focus happens to be.
    ##
    local target="${1}"
    assert-args target @RET

    local text="${claude_code_usage_type_continue_text:-Continue.}"

    if [[ "${target}" == codex:* ]] ; then
        #: Codex takes a message for a thread by name, which beats typing: it
        #: needs no window, no focus and no awake display, and it cannot land
        #: in the wrong place. See [agfi:h-codex-session-live-list] for where
        #: the thread id comes from.
        ensure-cmd codex @RET
        reval-ec command codex queue --thread "${target#codex:}" --message "${text}"
        return $?
    fi

    if [[ "${target}" == frontmost ]] ; then
        #: Wake the display first and give it a beat. `displaysleep` is ten
        #: minutes here -- the same as the idle threshold -- so by the time this
        #: fires the screen is asleep, and the first synthetic keypress would be
        #: eaten waking it, typing `ontinue.`
        silent h-hammerspoon-eval 'hs.caffeinate.declareUserActivity() ; return true' || true
        sleep 1

        #: Its sleep argument is mandatory, and 0 is right: the waiting was ours
        #: to do, by polling, so that a suspend could not skew it.
        hs-type-continue 0 @RET
        return 0
    fi

    if [[ ! "${target}" =~ '^kitty:[0-9]+$' ]] ; then
        ecerr "$0: unknown target: ${target}"
        return 1
    fi
    local id="${target#kitty:}"

    ensure-cmd kitty jq @RET

    local sock
    sock="$(h-claude-code-session-kitty-socket)" @RET

    #: `send-text` documents that it "always succeeds, even if no text was sent
    #: to any window", so its exit status proves nothing and the window has to
    #: be checked for separately. Without this a tab closed during the wait
    #: would swallow the resume while we reported success.
    if ! kitty @ --to "${sock}" ls |
            jq -e --argjson id "${id}" 'any(.[].tabs[].windows[] ; .id == $id)' >/dev/null ; then
        ecerr "$0: kitty window ${id} is gone"
        return 1
    fi

    kitty @ --to "${sock}" send-text --match "id:${id}" "${text}"$'\r' @RET
}

function h-claude-code-usage-notif-fire {
    #: What the armed job does once the deadline has passed: tell you, or resume
    #: the sessions that were blocked.
    ##
    local action="${claude_code_usage_notif_action:-notif}"
    local idle_min_s="${claude_code_usage_type_continue_idle_min_s:-600}"

    local profile="${1}" msg="${2}"
    assert-args profile msg @RET

    if [[ "${action}" != type-continue ]] ; then
        h-claude-code-usage-notif-notify "${msg}"
        h-claude-code-usage-notif-log "${profile}: notified"

        return 0
    fi

    local -a targets
    targets=(${=claude_code_usage_notif_targets})
    if (( ${#targets} == 0 )) ; then
        h-claude-code-usage-notif-notify "${msg} -- not resuming: no target was recorded"
        h-claude-code-usage-notif-log "${profile}: notified only, no target recorded"

        return 0
    fi

    #: Failing safe: anything we cannot establish means we do not type. The
    #: notification goes out either way, so an unwanted resume is the worse
    #: error of the two.
    local idle_s reason=''
    if ! idle_s="$(h-claude-code-usage-idle-s)" ; then
        reason='could not read the idle time'
    elif (( idle_s < idle_min_s )) ; then
        reason="you were at the keyboard ($(seconds-fmt-short "${idle_s}") idle, needs $(seconds-fmt-short "${idle_min_s}"))"
    elif h-claude-code-usage-screen-locked-p ; then
        reason='the screen is locked'
    fi

    if test -n "${reason}" ; then
        h-claude-code-usage-notif-notify "${msg} -- not resuming: ${reason}"
        h-claude-code-usage-notif-log "${profile}: notified only: ${reason}"

        return 0
    fi

    local target
    local -a resumed unreachable
    for target in "${targets[@]}" ; do
        if h-claude-code-usage-type-continue-send "${target}" ; then
            resumed+=("${target}")
        else
            unreachable+=("${target}")
        fi
    done

    local report="${msg}"
    if (( ${#resumed} )) ; then
        report+=" -- resumed ${(j:, :)resumed}"
    fi
    if (( ${#unreachable} )) ; then
        report+=" -- could not reach ${(j:, :)unreachable}"
    fi

    h-claude-code-usage-notif-notify "${report}"
    h-claude-code-usage-notif-log "${profile}: ${report}"
}

function h-claude-code-usage-type-continue-target-fz {
    #: Chooses what gets resumed, at ARM time, so the target is what you picked
    #: rather than whatever happens to hold the keyboard hours later. Prints one
    #: target per line: `kitty:<window-id>`, `codex:<thread-id>`, or
    #: `frontmost`.
    #:
    #: Every agent's live sessions are offered, not only Claude Code's: what
    #: the reset unblocks is often one conversation among several, and the
    #: waiting one may be a Codex thread told to hold off.
    ##
    #: `local` is dynamically scoped in zsh, so the picker sees these without
    #: anything being exported. The row layout is
    #: [agfi:h-agent-session-live-rows]'s.
    local -a agent_session_live_fz_extra_rows
    agent_session_live_fz_extra_rows=(
        $'frontmost\t-\t-\tfrontmost\t-\t-\t-\twhatever holds the keyboard when the limits reset'
    )

    #: No header: the picker's own advertises alt+enter, which converts a
    #: transcript to org. This picker is choosing what to resume, and that is
    #: noise here. The binding still works, it is just not announced.
    local agent_session_fz_header=''

    local selected
    selected="$(agent-session-live-fz)" @RET

    #: `(ps:\t:)' rather than `read': tab is IFS whitespace, so `read' would
    #: collapse an empty field rather than keep the columns lined up.
    local line agent
    local -a f
    while IFS= read -r line ; do
        test -n "${line}" || continue
        f=( "${(@ps:\t:)line}" )

        if [[ "${f[1]}" == frontmost ]] ; then
            ec frontmost
            continue
        fi

        #: A Codex thread is reachable without a window at all, which is
        #: strictly better; anything else is typed into its kitty window.
        agent="$(h-agent-session-agent-of "${f[2]}" 2>/dev/null)" || agent=''
        if [[ "${agent}" == codex ]] ; then
            ec "codex:$(h-agent-session-call codex id-of "${f[2]}")"
            continue
        fi

        test -n "${f[1]}" && [[ "${f[1]}" != '-' ]] || continue
        ec "kitty:${f[1]}"
    done <<< "${selected}"
}

function h-claude-code-usage-notif {
    #: Arms, or re-arms, a one-shot notification for when the limits that
    #: currently block us have reset.
    #:
    #: $1 is the tmux session to live in, $2 the profile to read, and the rest
    #: the roles this variant cares about (see
    #: [agfi:h-claude-code-usage-notif-window]).
    #:
    #: Re-arming cannot stack: [agfi:tmuxnew] kills the previous session's
    #: processes before creating the replacement, so the session name alone
    #: guarantees a single pending notifier -- no lock, marker or redis key.
    #: The tmux server is also independent of the brish garden, so
    #: =brishz-restart= does not silently disarm it. A reboot does.
    ##
    local poll_s="${claude_code_usage_notif_poll_s:-30}"
    local full_pct="${claude_code_usage_notif_full_pct:-100}"
    local action="${claude_code_usage_notif_action:-notif}"

    #: Typing gets the longer grace of the two; see the knobs above.
    local grace_s="${claude_code_usage_notif_grace_s:-30}"
    if [[ "${action}" == type-continue ]] ; then
        grace_s="${claude_code_usage_type_continue_grace_s:-60}"
    fi

    local session="${1}" profile="${2}"
    assert-args session profile @RET
    local roles=("${@[3,-1]}")
    assert-args roles @RET

    ensure-cmd jq tmux @RET
    zmodload zsh/datetime 2>/dev/null

    #: =claude_code_usage_notif_p=n= is the recursion guard, and load-bearing:
    #: the report arms the notifier and the notifier reads the report.
    local json
    json="$(claude_code_usage_notif_p=n claude_code_usage_json_p=y claude_code_usage_profile="${profile}" claude-code-usage)" @RET

    local blocked_labels=() role out pct resets label
    integer blocked_at=0
    for role in "${roles[@]}" ; do
        if ! out="$(h-claude-code-usage-notif-window "${json}" "${role}")" ; then
            ecgray "$0: ${profile}: no ${role} window, skipping"
            continue
        fi

        pct="${out%%$'\t'*}"
        resets="${${out#*$'\t'}%%$'\t'*}"
        label="${out##*$'\t'}"

        if (( pct >= full_pct )) && (( resets > 0 )) ; then
            blocked_labels+=("${label}")

            #: The LATEST reset among the blocked windows is when we are
            #: actually free again: a 5h rollover buys nothing while the
            #: weekly limit is still spent.
            if (( resets > blocked_at )) ; then
                blocked_at=${resets%.*}
            fi
        fi
    done

    integer deadline=0
    local msg=''
    if (( ${#blocked_labels} == 0 )) ; then
        if ! isDeus ; then
            ecgray "$0: ${profile}: usage already possible, not arming (use \`deus\` to arm anyway)"
            return 0
        fi

        #: deus: arm for the next 5h rollover anyway, so the mechanism can be
        #: exercised without having to be rate-limited first.
        out="$(h-claude-code-usage-notif-window "${json}" session)" @RET
        deadline=${${${out#*$'\t'}%%$'\t'*}%.*}
        msg="Claude Code (${profile}): ${out##*$'\t'} window rolled over"
    else
        deadline=${blocked_at}
        msg="Claude Code (${profile}): ${(j:, :)blocked_labels} reset, usage available again"
    fi

    deadline=$(( deadline + grace_s ))

    if (( deadline <= EPOCHSECONDS )) ; then
        ecgray "$0: ${profile}: reset time is already past (stale data?), not arming"
        return 0
    fi

    #: Only now that we know we are going to arm, so a report that changes
    #: nothing never puts a picker in your way. Presetting the variable skips
    #: it, which is what makes this callable from a script or a test.
    local targets="${claude_code_usage_notif_targets}"
    if [[ "${action}" == type-continue ]] && test -z "${targets}" ; then
        local -a target_list
        target_list=("${(@f)$(h-claude-code-usage-type-continue-target-fz)}") @TRET
        #: Space separated, because that is what survives the trip into the
        #: tmux session's environment intact.
        targets="${(j: :)target_list}"

        if test -z "${targets}" ; then
            ecgray "$0: ${profile}: no resume target chosen, not arming"
            return 0
        fi
    fi

    ecgray "$0: arming ${session} for $(date-unix-to-3339 "${deadline}") (in $(seconds-fmt-short $(( deadline - EPOCHSECONDS ))))"

    #: =silent= because [agfi:tmux-session-processes-kill] narrates every
    #: re-arm, which would otherwise land in the middle of a usage report.
    silent tmuxnewsh2 "${session}" \
        claude_code_usage_notif_poll_s="${poll_s}" \
        claude_code_usage_notif_action="${action}" \
        claude_code_usage_notif_targets="${targets}" \
        h-claude-code-usage-notif-wait "${deadline}" "${profile}" "${msg}" @RET

    #: Recorded on the tmux session itself rather than in redis, so the
    #: bookkeeping cannot drift from whether the job actually exists.
    #:
    #: No `=` exact-match prefix on the target here: unlike =has-session=,
    #: =set-option= does not accept one and fails with "no such session".
    silent tmux set-option -t "${session}" '@ccu_notif_deadline' "${deadline}" || true
    silent tmux set-option -t "${session}" '@ccu_notif_action' "${action}" || true
    silent tmux set-option -t "${session}" '@ccu_notif_targets' "${targets}" || true
    #: The name is how this job is found and re-armed; the Claude Code it
    #: resumes must not rename it ([agfi:claude-code-session-tmux-autoname]).
    silent tmux set-option -t "${session}" "${agent_tmux_autoname_option}" off || true
}

function h-claude-code-usage-notif-for-profile {
    local profile="${1}"
    assert-args profile @RET

    local session
    session="$(h-claude-code-usage-notif-session "${profile}")" @RET

    h-claude-code-usage-notif "${session}" "${profile}" session weekly_all
}

#: Scheduling entry points. These schedule a notifier and print no report,
#: which is not the intended way in -- the =-notify= reports are -- so they are
#: =h-=. They stay callable as an escape hatch for when you already have a
#: report in front of you.
function h-claude-code-usage-notif-schedule {
    h-claude-code-usage-notif-for-profile default
}

function h-claude-code-usage-work-notif-schedule {
    h-claude-code-usage-notif-for-profile work
}

function h-claude-code-usage-fable-notif-schedule {
    #: The weekly Fable window as well as the windows that block everything.
    #: Its own tmux session, so it can be scheduled alongside the default
    #: profile's notifier rather than replacing it.
    ##
    h-claude-code-usage-notif 'claude-code-usage-fable-notif-schedule' default \
        session weekly_all 'weekly:Fable'
}

function claude-code-usage-fable-notify {
    #: The default profile's report, then schedules the *Fable* watcher rather
    #: than the profile one. Fable is not a profile -- it is an extra window on
    #: the default profile -- so it cannot be reached by setting
    #: =claude_code_usage_notif_p=, and needs its own entry point.
    ##
    local retcode=0
    claude_code_usage_notif_p=n claude-code-usage "$@" || retcode=$?

    if (( retcode == 0 )) ; then
        #: =>&2= because our stdout may be a JSON document a caller is about to
        #: parse; non-fatal because a failed schedule must not make a working
        #: report look broken.
        h-claude-code-usage-fable-notif-schedule >&2 || true
    fi

    return "${retcode}"
}

##
#: Resuming rather than merely announcing: the profile's ordinary report, plus
#: an arm whose action is to type into the session that was blocked. The target
#: is picked interactively at arm time -- see
#: [agfi:h-claude-code-usage-type-continue-target-fz] -- because
#: [agfi:hs-type-continue] types wherever the keyboard focus is, and several
#: Claude sessions are usually open at once.
aliasfnq claude-code-usage-type-continue claude_code_usage_notif_p=y claude_code_usage_notif_action=type-continue claude-code-usage
aliasfn cctc claude-code-usage-type-continue

aliasfnq claude-code-usage-default-type-continue claude_code_usage_notif_p=y claude_code_usage_notif_action=type-continue claude-code-usage-default
aliasfn cctc-default claude-code-usage-default-type-continue

aliasfnq claude-code-usage-work-type-continue claude_code_usage_notif_p=y claude_code_usage_notif_action=type-continue claude-code-usage-work
aliasfn cctc-work claude-code-usage-work-type-continue

#: Scheduling without a report, matching the =h-...-notif-schedule= escape
#: hatches above.
function h-claude-code-usage-type-continue-schedule {
    claude_code_usage_notif_action=type-continue h-claude-code-usage-notif-for-profile default
}

function h-claude-code-usage-work-type-continue-schedule {
    claude_code_usage_notif_action=type-continue h-claude-code-usage-notif-for-profile work
}
##
function claude-code-usage-notif-sessions {
    #: Every tmux session a notifier can live in, one per line.
    local profile out=()
    for profile in "${claude_code_profile_order[@]}" ; do
        out+=("$(h-claude-code-usage-notif-session "${profile}")")
    done
    out+=('claude-code-usage-fable-notif-schedule')

    ec "${(F)out}"
}

function claude-code-usage-notif-cancel {
    local sessions=("$@")
    if (( ${#sessions} == 0 )) ; then
        sessions=("${(@f)$(claude-code-usage-notif-sessions)}")
    fi

    local s alive_p
    for s in "${sessions[@]}" ; do
        if ! silent tmux has-session -t "=${s}" ; then
            continue
        fi

        alive_p=n
        if tmux-alive-p "${s}" ; then
            alive_p=y
        fi

        #: Dead sessions get reaped too. With =remain-on-exit= on, a notifier
        #: that has already fired leaves its session behind, and clearing those
        #: out is what someone running a cancel actually wants.
        silent tmux-session-processes-kill "${s}"
        if bool "${alive_p}" ; then
            ecgray "$0: cancelled ${s}"
        else
            ecgray "$0: reaped ${s}, which had already fired"
        fi
    done
}

function claude-code-usage-notif-status {
    zmodload zsh/datetime 2>/dev/null

    local s deadline
    integer remaining
    for s in "${(@f)$(claude-code-usage-notif-sessions)}" ; do
        if ! tmux-alive-p "${s}" ; then
            #: With =remain-on-exit= on a fired notifier leaves its session
            #: behind, which answers "did my notification actually go off?".
            if silent tmux has-session -t "=${s}" ; then
                ecgray "${s}: not armed; a previous notifier has already fired"
            else
                ecgray "${s}: not armed"
            fi

            continue
        fi

        deadline="$(tmux show-options -qv -t "${s}" '@ccu_notif_deadline' 2>/dev/null)" || deadline=''

        #: Which action is pending matters as much as when: arming a resume
        #: replaces a plain notifier for that profile, and the reverse, so a
        #: downgrade should be visible rather than silent.
        local action targets suffix=''
        action="$(tmux show-options -qv -t "${s}" '@ccu_notif_action' 2>/dev/null)" || action=''
        targets="$(tmux show-options -qv -t "${s}" '@ccu_notif_targets' 2>/dev/null)" || targets=''
        if test -n "${action}" ; then
            suffix=" [action: ${action}"
            if test -n "${targets}" ; then
                suffix+=" -> ${targets}"
            fi
            suffix+=']'
        fi
        if test -z "${deadline}" ; then
            ec "${s}: armed (no deadline recorded)${suffix}"
            continue
        fi

        remaining=$(( deadline - EPOCHSECONDS ))
        if (( remaining > 0 )) ; then
            ec "${s}: armed for $(date-unix-to-3339 "${deadline}") (in $(seconds-fmt-short ${remaining}))${suffix}"
        else
            ec "${s}: armed, but its deadline passed $(seconds-fmt-short $(( -remaining ))) ago${suffix}"
        fi
    done
}
##
function claude-work {
    #: Claude Code on the work seat: a second config home, so a separate
    #: account, history, projects and plugins. `settings.json` there symlinks
    #: to the same tracked file as the personal profile.
    #:
    #: Goes through [agfi:claude] rather than `command claude`, so it gets
    #: [agfi:h-agents-md-sync-ask] -- which is what keeps
    #: ~/.claude-work/CLAUDE.md current -- along with the watchdogs, the retry
    #: cap and `$proxyenv`. Two consequences worth knowing: `$proxyenv` now
    #: applies to work sessions too (a no-op unless proxy mode is on), and the
    #: exported CLAUDE_CONFIG_DIR is inherited, so anything the session
    #: launches -- [agfi:claude-autocommit], [agfi:claude-vcsh-commit] -- stays
    #: on this seat.
    #:
    #: The personal profile is deliberately *not* pinned the same way. Claude
    #: Code hashes the config dir into the keychain service name, using a bare
    #: `Claude Code-credentials` only while CLAUDE_CONFIG_DIR is unset, so
    #: setting it there would cost a re-login for nothing.
    ##
    #: The visual cues -- glyph, theme, tint, tmux label -- are not set here.
    #: [agfi:claude] derives all of them from this config dir via
    #: =claude_code_profile_markers= and friends, so a work session looks like
    #: one however it was started, including a bare `CLAUDE_CONFIG_DIR=... claude`.
    local -x CLAUDE_CONFIG_DIR="${HOME}/.claude-work"

    claude "$@"
}
##
