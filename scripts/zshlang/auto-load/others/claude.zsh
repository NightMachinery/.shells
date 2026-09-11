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

    #: Two ways to recognise our own row, and either is enough. The window
    #: option is the one [agfi:h-claude-tmux-label-set] writes; the format is
    #: recognisable on its own because it names our pane option, and that is
    #: what covers a window whose row was set by a session older than the
    #: marker -- or one that lost it.
    local ours format
    ours="$(command tmux show-options -wqv -t "${TMUX_PANE}" "${claude_tmux_label_row_option}" 2>/dev/null)"
    format="$(command tmux show-options -wqv -t "${TMUX_PANE}" pane-border-format 2>/dev/null)"
    if test -z "${ours}" && [[ "${format}" != *"${claude_tmux_label_option}"* ]] ; then
        return 0
    fi

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
    #: Whether an expired keychain credential (or a 401) may be refreshed by
    #: running Claude Code's own print-mode =/usage= once for that profile. On
    #: by default: it is a built-in, so it spends no tokens and starts no
    #: conversation. Turn it off where spawning =claude= is unwelcome.
    local relogin_p="${claude_code_usage_relogin_p:-y}"

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
    #: Only the off switch is passed: the script's own default is on, and
    #: saying so twice is how the two drift apart.
    if ! bool "${relogin_p}" ; then
        args+=(--no-relogin)
    fi

    ec "${(F)args}"
}

function h-claude-code-usage-token-env-load {
    #: Prints =typeset -x= lines for every named profile that has a token file
    #: at =~/.keys/claude-code-oauth-<profile>=, for the caller to =eval=.
    #: Usage: eval "$(h-claude-code-usage-token-env-load <profile>...)"
    #:
    #: A long-lived token (minted with =claude setup-token=) is what makes the
    #: report work in a GUI-detached session with no garden to delegate to.
    #:
    #: =eval= rather than an argv prefix, and scoped to the caller rather than
    #: exported globally: a token in a command's arguments is visible in =ps=
    #: to anyone on the machine, and would be recorded in the garden's command
    #: log whenever the report is delegated.
    ##
    local profile var file
    for profile in "$@" ; do
        file="${HOME}/.keys/claude-code-oauth-${profile}"
        test -r "${file}" || continue

        var="CLAUDE_CODE_OAUTH_TOKEN_${${(U)profile}//[^A-Z0-9]/_}"
        #: A variable already in the environment is a deliberate override of
        #: whatever is on disk, so it wins.
        test -n "${(P)var}" && continue

        ec "typeset -x ${var}=${(q)$(<${file})}"
    done
}

function h-claude-code-usage-garden-p {
    #: Whether to run the report inside the brish garden rather than here.
    #:
    #: The garden's worker shells are attached to the GUI session, so they can
    #: read the login keychain. A GUI-detached session cannot: its keychain
    #: search list collapses to the System keychain alone, so the lookup finds
    #: nothing and =security= reports the credential as simply absent. See
    #: =docs/claude_code_usage.md=.
    #:
    #: Proactive rather than a retry, because over ssh the local read *cannot*
    #: succeed -- attempting it first would only buy a round trip and a
    #: misleading error.
    ##
    local mode="${claude_code_usage_garden_p:-auto}"

    case "${mode}" in
        y) return 0 ;;
        n) return 1 ;;
        auto) isSSH ;;
        *)
            ecerr "$0: unknown claude_code_usage_garden_p: ${mode} (auto, y, n)"
            return 1
            ;;
    esac
}

function h-claude-code-usage-run {
    #: The one place the script is invoked, so [agfi:claude-code-usage] and
    #: [agfi:claude-code-usage-all] cannot drift apart and the garden fallback
    #: covers both.
    #:
    #: The whole report is delegated, not just the keychain read, so the token
    #: never leaves the GUI-attached process: only the rendered report or the
    #: JSON payload comes back.
    ##
    if h-claude-code-usage-garden-p && brishz-alive-p ; then
        local -a color_opts
        #: The garden's stdout is a pipe, so =--color auto= would resolve to no
        #: colour for the command run most often. Placed first, so an explicit
        #: =--color= from the caller still wins (argparse is last-wins).
        [[ -t 1 ]] && color_opts=(--color always)

        #: Non-ASCII survives the trip this way; the inline transport mangles
        #: it, and the report is full of box drawing.
        brishz_out_file_p=y brishzq.zsh claude_code_usage.py "${color_opts[@]}" "$@"
        return $?
    fi

    $proxyenv revaldbg command claude_code_usage.py "$@"
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

    eval "$(h-claude-code-usage-token-env-load "${profile}")"

    local script_args=(
        --profile-label "${profile}"
        --config-dir "${claude_code_profiles[$profile]}"
        "${common[@]}"
    )

    #: =script_args= before user args so explicit CLI flags win (argparse last-wins).
    local retcode=0
    h-claude-code-usage-run "${script_args[@]}" "$@" || retcode=$?

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

    eval "$(h-claude-code-usage-token-env-load "${profiles[@]}")"

    local script_args=(--all "${common[@]}")
    local p
    for p in "${profiles[@]}" ; do
        h-claude-code-profile-assert "${p}" @RET

        script_args+=(--profile "${p}=${claude_code_profiles[$p]}")
    done

    local retcode=0
    h-claude-code-usage-run "${script_args[@]}" "$@" || retcode=$?

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
#: Utilization at or above which a window counts as blocking us. The only knob
#: of the notifier that is Claude Code's own: everything about waiting the
#: reset out and resuming afterwards is agent-neutral and lives in
#: =agent-usage.zsh= under `agent_usage_*'. See =docs/agent-usage-notif.md=.
typeset -g claude_code_usage_notif_full_pct="${claude_code_usage_notif_full_pct:-100}"

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

function h-claude-code-usage-notif {
    #: Arms, or re-arms, a one-shot notification for when the limits that
    #: currently block us have reset.
    #:
    #: $1 is the tmux session to live in, $2 the profile to read, and the rest
    #: the roles this variant cares about (see
    #: [agfi:h-claude-code-usage-notif-window]).
    #:
    #: Only the *when* and the *what to say* are worked out here. Waiting the
    #: reset out, deciding whether resuming is safe and delivering the resume
    #: text are agent-neutral, and belong to
    #: [agfi:h-agent-usage-notif-arm] -- grace, the picker, the tmux job and
    #: its bookkeeping included. See =docs/agent-usage-notif.md=.
    ##
    local full_pct="${claude_code_usage_notif_full_pct:-100}"

    local session="${1}" profile="${2}"
    assert-args session profile @RET
    local roles=("${@[3,-1]}")
    assert-args roles @RET

    ensure-cmd jq @RET

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

    integer reset_at=0
    local msg=''
    if (( ${#blocked_labels} == 0 )) ; then
        if ! isDeus ; then
            ecgray "$0: ${profile}: usage already possible, not arming (use \`deus\` to arm anyway)"
            return 0
        fi

        #: deus: arm for the next 5h rollover anyway, so the mechanism can be
        #: exercised without having to be rate-limited first.
        out="$(h-claude-code-usage-notif-window "${json}" session)" @RET
        reset_at=${${${out#*$'\t'}%%$'\t'*}%.*}
        msg="Claude Code (${profile}): ${out##*$'\t'} window rolled over"
    else
        reset_at=${blocked_at}
        msg="Claude Code (${profile}): ${(j:, :)blocked_labels} reset, usage available again"
    fi

    #: The seat whose limit we are waiting on, so the tmux picker offers only
    #: sessions on that seat: a work reset must not resume a personal one.
    #: `local' is dynamically scoped in zsh, so the picker sees it without
    #: anything being exported.
    #:
    #: `agent_session_agents' is deliberately left alone, so the kitty picker
    #: keeps offering every agent: what the reset unblocks is often one
    #: conversation among several. See
    #: [agfi:h-agent-usage-continue-targets-kitty-fz].
    local agent_usage_continue_profile="${profile}"

    h-agent-usage-notif-arm "${session}" "${reset_at}" "${msg}"
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
#: an arm whose action is to pick the conversation back up once the limits
#: lift. Named after the *delivery mechanism*, because that is the whole of
#: what distinguishes them and it is what decides whether a resume can reach
#: you at all:
#:
#:   -continue-kitty-fz   pick kitty windows; typed into with `kitty @ send-text'
#:   -continue-tmux-fz    pick tmux panes; typed into with `send-keys'
#:   -continue-frontmost  no picker; typed wherever the focus is at the time
#:
#: A Codex thread picked in either picker is *queued* instead of typed into,
#: which needs no window at all; see
#: [agfi:h-agent-usage-continue-rows-to-targets].
#:
#: Generated rather than written out nine times: three mechanisms by the bare
#: entry point plus the two named seats, and the only thing that varies is one
#: knob and one function name.
##
() {
    local via profile base name
    for via in kitty-fz tmux-fz frontmost ; do
        for profile in '' default work ; do
            base="claude-code-usage${profile:+-${profile}}"
            name="${base}-continue-${via}"

            #: `${via%-fz}' because the name advertises the picker and the knob
            #: names the mechanism: `-continue-tmux-fz' arms
            #: `agent_usage_continue_via=tmux'.
            aliasfnq "${name}" \
                claude_code_usage_notif_p=y \
                agent_usage_notif_action=continue \
                agent_usage_continue_via="${via%-fz}" \
                "${base}"
        done
    done
}

#: The short names, mirroring the =ccu-default= block above.
aliasfn cck claude-code-usage-continue-kitty-fz
aliasfn cck-default claude-code-usage-default-continue-kitty-fz
aliasfn cck-work claude-code-usage-work-continue-kitty-fz

aliasfn cct claude-code-usage-continue-tmux-fz
aliasfn cct-default claude-code-usage-default-continue-tmux-fz
aliasfn cct-work claude-code-usage-work-continue-tmux-fz

aliasfn ccfront claude-code-usage-continue-frontmost
aliasfn ccfront-default claude-code-usage-default-continue-frontmost
aliasfn ccfront-work claude-code-usage-work-continue-frontmost

function h-claude-code-usage-continue-schedule {
    #: Arming a resume without printing a report, matching the
    #: =h-...-notif-schedule= escape hatches above. One hatch for all three
    #: mechanisms, since `agent_usage_continue_via' already names them and a
    #: function per mechanism would say nothing the knob does not.
    #: Usage: h-claude-code-usage-continue-schedule [profile]
    ##
    local profile="${1:-default}"

    agent_usage_notif_action=continue h-claude-code-usage-notif-for-profile "${profile}"
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

#: Cancelling and reporting are the same act whatever armed the job, so both
#: are [agfi:h-agent-usage-notif-cancel] and [agfi:h-agent-usage-notif-status]
#: over the sessions this profile family owns. Named arguments still narrow it
#: to one session.
function claude-code-usage-notif-cancel {
    local sessions=("$@")
    if (( ${#sessions} == 0 )) ; then
        sessions=("${(@f)$(claude-code-usage-notif-sessions)}")
    fi

    h-agent-usage-notif-cancel "${sessions[@]}"
}

function claude-code-usage-notif-status {
    local sessions=("$@")
    if (( ${#sessions} == 0 )) ; then
        sessions=("${(@f)$(claude-code-usage-notif-sessions)}")
    fi

    h-agent-usage-notif-status "${sessions[@]}"
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
