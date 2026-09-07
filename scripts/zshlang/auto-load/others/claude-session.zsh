##
#: Rendering and session scanning live in =golang/claude_session=; these are
#: thin wrappers around it. See =golang/claude_session/readme.org=.
##
function h-claude-code-session-dep {
    #: Ensures the renderer is built and on PATH, building it on first use.
    ##
    ensure-cmd go @RET
    ensure-dep1 claude_session go-install-local "${NIGHTDIR}/golang/claude_session" @RET
}

function h-claude-code-session-name {
    #: The session's own name: the title the user set, else the one Claude
    #: Code generated, else its slug, else its UUID for sessions predating
    #: all of those. Sanitized for use as a filename.
    ##
    local input="${1}"

    h-claude-code-session-dep @RET

    local name
    name="$(claude_session name "${input}")" @RET
    name="${name//[^A-Za-z0-9._-]/-}"
    #: Collapse the runs a title's spaces and punctuation leave behind.
    name="${${name//---##/-}%%-##}"
    name="${name##-##}"

    if test -z "${name}" ; then
        ec "${input:t:r}"
    else
        ec "${name}"
    fi
}

function h-claude-code-session-title {
    #: Emits the document header for a session, in the given syntax.
    #: Usage: h-claude-code-session-title <org|md> <input>
    ##
    local syntax="${1}" input="${2}"

    local id="${input:t:r}"
    local name
    name="$(h-claude-code-session-name "${input}")" @RET

    if [[ "${syntax}" == org ]] ; then
        if [[ "${name}" == "${id}" ]] ; then
            ec "#+TITLE: Claude Code Session ${id}"
        else
            ec "#+TITLE: ${name}"
            ec "#+SUBTITLE: Claude Code session ${id}"
        fi
    else
        if [[ "${name}" == "${id}" ]] ; then
            ec "# Claude Code Session ${id}"
        else
            ec "# ${name}"
            ec
            ec "Claude Code session ${id}"
        fi
    fi
    ec
}

function h-claude-code-session-render {
    #: Renders a Claude Code session `.jsonl` to stdout.
    #: Usage: h-claude-code-session-render <format> <input>
    ##
    local format="${1}" input="${2}"

    h-claude-code-session-dep @RET
    if [[ "${format}" == org-pandoc ]] ; then
        ensure-cmd pandoc @RET
    fi

    local render_args=("-format=${format}")
    local max_lines="${claude_code_session_max_block_lines:-0}"
    render_args+=("-max-block-lines=${max_lines}")
    if bool "${claude_code_session_diff_p:-y}" ; then
        render_args+=(-diff)
    else
        render_args+=(-diff=false)
    fi
    if bool "${claude_code_session_subagents_p:-y}" ; then
        render_args+=(-subagents)
    else
        render_args+=(-subagents=false)
    fi

    assert claude_session render "${render_args[@]}" "${input}" @RET
}

function h-claude-code-session-to-md {
    #: Converts a Claude Code session `.jsonl` file into a markdown file.
    ##
    local input="${1}"
    local out="${2:-${input:r}.md}"

    if ! test -e "${input}" ; then
        ecerr "$0: input file does not exist: ${input}"
        return 1
    fi

    {
        h-claude-code-session-title md "${input}" @RET
        h-claude-code-session-render md "${input}" @RET
    } > "${out}"
}

function h-claude-code-session-to-org-native {
    #: Converts a Claude Code session `.jsonl` file into an org-mode file,
    #: without pandoc. Message bodies stay markdown, so this is only a
    #: fallback; prefer [agfi:h-claude-code-session-to-org-pandoc].
    ##
    local input="${1}"
    local out="${2:-${input:r}.org}"

    if ! test -e "${input}" ; then
        ecerr "$0: input file does not exist: ${input}"
        return 1
    fi

    {
        h-claude-code-session-title org "${input}" @RET
        h-claude-code-session-render org "${input}" @RET
    } > "${out}"
}

function h-claude-code-session-to-org-pandoc {
    #: Converts a Claude Code session `.jsonl` file into an org-mode file.
    #: Emits intermediate markdown and lets pandoc do the org conversion,
    #: so the markdown message bodies become proper org markup.
    ##
    local input="${1}"
    local out="${2:-${input:r}.org}"

    if ! test -e "${input}" ; then
        ecerr "$0: input file does not exist: ${input}"
        return 1
    fi

    #: The pandoc run happens inside the renderer, split across processes;
    #: see "Performance" in =golang/claude_session/readme.org=.
    {
        h-claude-code-session-title org "${input}" @RET
        h-claude-code-session-render org-pandoc "${input}" @RET
    } > "${out}"
}
aliasfn h-claude-code-session-to-org h-claude-code-session-to-org-pandoc

function h-claude-code-session-projects-dirs {
    #: Every Claude Code profile's projects directory, one per line.
    #:
    #: Claude Code keeps its state under `$CLAUDE_CONFIG_DIR`, and
    #: [agfi:claude-work] runs a second config home for the work account, so
    #: there is more than one of these and a session started there is
    #: otherwise invisible to the picker. A glob rather than a written-out
    #: list, so a third profile needs no wiring -- the same reasoning as the
    #: socket glob in [agfi:h-claude-code-session-kitty-socket].
    ##
    ensure-array claude_code_session_projects_dirs
    if (( ${#claude_code_session_projects_dirs} )) ; then
        print -rl -- "${claude_code_session_projects_dirs[@]}"
        return 0
    fi

    #: The older singular name, so an existing override still works.
    if test -n "${claude_code_view_session_fz_projects_dir}" ; then
        ec "${claude_code_view_session_fz_projects_dir}"
        return 0
    fi

    local -a dirs
    dirs=( ${~${claude_code_session_projects_dirs_glob:-${HOME}/.claude*/projects}}(N/) )
    if (( ${#dirs} == 0 )) ; then
        ecerr "$0: no Claude Code projects directory found"
        return 1
    fi

    print -rl -- "${dirs[@]}"
}

function h-claude-code-session-select-fz {
    #: Interactively selects a Claude Code session `.jsonl` file and
    #: prints its path.
    ##
    local scope="${claude_code_view_session_fz_scope:-project}"
    ensure-array claude_code_view_session_fz_fz_opts
    local fz_opts=("${claude_code_view_session_fz_fz_opts[@]}")

    local -a projects_dirs
    projects_dirs=("${(@f)$(h-claude-code-session-projects-dirs)}") @TRET

    #: Every profile's copy of this project, not just the personal one.
    local -a wanted
    local d
    if [[ "${scope}" == "all" ]] ; then
        wanted=("${projects_dirs[@]}")
    else
        #: Claude Code's rule: every non-alphanumeric character becomes `-',
        #: not only slashes and dots. `~/notes/x y' and `~/tmp/_cdtmp_1' were
        #: looked up under the wrong name before.
        local project_dir_name="${PWD//[^[:alnum:]]/-}"
        for d in "${projects_dirs[@]}" ; do
            wanted+=("${d}/${project_dir_name}")
        done
    fi

    #: A profile that has never been used in this directory simply has no such
    #: directory, which is normal rather than an error.
    local -a sessions_dirs
    for d in "${wanted[@]}" ; do
        if test -d "${d}" ; then
            sessions_dirs+=("${d}")
        fi
    done

    if (( ${#sessions_dirs} == 0 )) ; then
        ecerr "$0: no sessions directory exists for scope '${scope}':"
        ecerr "  ${(j: :)wanted}"
        return 1
    fi

    h-claude-code-session-dep @RET

    #: `epoch<TAB>path<TAB>local time<TAB>relative path<TAB>snippet`, newest
    #: first. The time is the last message's, not the file's mtime; see
    #: =golang/claude_session/readme.org=.
    local list_args=()
    if bool "${claude_code_view_session_fz_subagents_p:-n}" ; then
        #: Off by default: subagent transcripts are inlined into their parent
        #: by the renderer, so listing them here too is noise.
        list_args+=(-subagents)
    fi

    #: `list` merges the roots and sorts across all of them, and labels each
    #: relative path with its profile when there is more than one.
    local lines
    lines="$(claude_session list "${list_args[@]}" "${sessions_dirs[@]}")" @RET

    local selected
    selected="$(ec "${lines}" | fz --delimiter=$'\t' --with-nth='3..' --no-multi "${fz_opts[@]}")" @RET
    selected="${selected%%$'\n'*}"

    local session_file="${${selected#*$'\t'}%%$'\t'*}"
    if ! test -e "${session_file}" ; then
        ecerr "$0: selected session file does not exist: ${session_file}"
        return 1
    fi

    ec "${session_file}"
}

function h-claude-code-view-session {
    #: Converts the given Claude Code session using the given converter
    #: function, and opens the result in emacs.
    #: Usage: h-claude-code-view-session <converter> <ext> <session-file>
    ##
    local converter="${1}"
    local ext="${2}"
    local session_file="${3}"

    if ! test -e "${session_file}" ; then
        ecerr "$0: session file does not exist: ${session_file}"
        return 1
    fi

    local tmp_dir
    tmp_dir="$(gmktemp --directory)" @TRET

    #: Named after the session, so the emacs buffer is recognizable. The id
    #: disambiguates the (unlikely) case of two sessions sharing a name.
    local name
    name="$(h-claude-code-session-name "${session_file}")" @RET

    local out_file="${tmp_dir}/${name}.${ext}"
    if test -e "${out_file}" ; then
        out_file="${tmp_dir}/${name}-${${session_file:t:r}[1,8]}.${ext}"
    fi
    "${converter}" "${session_file}" "${out_file}" @RET

    emc-open "${out_file}" @RET
}

function h-claude-code-view-session-fz {
    #: Interactively selects a Claude Code session, converts it using the
    #: given converter function, and opens the result in emacs.
    ##
    local converter="${1}"
    local ext="${2}"

    local session_file
    session_file="$(h-claude-code-session-select-fz)" @RET

    h-claude-code-view-session "${converter}" "${ext}" "${session_file}"
}

function claude-code-view-session {
    #: Converts the given Claude Code session `.jsonl` to org-mode and opens
    #: it in emacs. The non-interactive counterpart of
    #: [agfi:claude-code-view-session-fz].
    ##
    h-claude-code-view-session h-claude-code-session-to-org org "${1}"
}

function claude-code-view-session-fz {
    #: Interactively selects a Claude Code session, converts it to
    #: org-mode, and opens it in emacs.
    ##
    h-claude-code-view-session-fz h-claude-code-session-to-org org @RET
}
#: Same, but selects from the sessions of all projects.
aliasfn claude-code-view-session-all-fz claude_code_view_session_fz_scope=all claude-code-view-session-fz

function claude-code-view-session-md-fz {
    #: Interactively selects a Claude Code session, converts it to
    #: markdown, and opens it in emacs.
    ##
    h-claude-code-view-session-fz h-claude-code-session-to-md md @RET
}
#: Same, but selects from the sessions of all projects.
aliasfn claude-code-view-session-md-all-fz claude_code_view_session_fz_scope=all claude-code-view-session-md-fz

function claude-code-view-session-raw-fz {
    #: Interactively selects a Claude Code session and opens the original
    #: `.jsonl` file in emacs.
    ##
    local session_file
    session_file="$(h-claude-code-session-select-fz)" @RET

    emc-open "${session_file}" @RET
}
#: Same, but selects from the sessions of all projects.
aliasfn claude-code-view-session-raw-all-fz claude_code_view_session_fz_scope=all claude-code-view-session-raw-fz
##
#: Reading the session you are *sitting in* should not need a picker: several
#: sessions often share a project directory, so "the newest one for this cwd"
#: is not reliably the right one. Claude Code cannot bind a key to a shell
#: command -- `keybindings.json` only takes a fixed action enum -- so the
#: keypress lives in kitty, and the hooks below leave it a note saying which
#: session runs in which window.
##
function h-claude-code-session-registry-dir {
    #: Where [agfi:claude-code-session-register] records which kitty window a
    #: Claude Code session was last showing in.
    #:
    #: Not under `~/tmp' any more. That directory gets swept, which is how
    #: kitty's remote-control socket died (see docs/unix-sockets.md), and this
    #: registry vanished in the same sweep.
    ##
    ec "${claude_code_session_registry_dir:-${XDG_STATE_HOME:-${HOME}/.local/state}/claude-code-sessions}"
}

function h-claude-code-session-registry-key {
    #: kitty numbers its windows from 1 again every time it restarts, so its
    #: pid is what keeps a dead kitty's entries from being read as live ones.
    #: Usage: h-claude-code-session-registry-key <kitty-pid> <kitty-window-id>
    ##
    local kpid="${1}" win="${2}"

    if test -z "${kpid}" || test -z "${win}" ; then
        return 1
    fi

    ec "${kpid}-${win}"
}

function claude-code-session-register {
    #: Records which kitty window the calling Claude Code session is showing in,
    #: so [agfi:claude-code-view-session-focused] has something to fall back on
    #: when nothing else can tell. For Claude Code's `SessionStart' and
    #: `UserPromptSubmit' hooks; the payload is JSON, taken from `$2' or stdin.
    #:
    #: This is the insurance layer. The three live lookups in
    #: [agfi:h-claude-code-session-of-kitty-window] cover every way a session
    #: reaches a kitty window today, with no bookkeeping at all. This is for the
    #: way that does not exist yet, and for the day the agent view stops putting
    #: the session's name in the window title.
    #:
    #: It records the one fact a hook has that nothing else does. When
    #: `UserPromptSubmit' fires, Enter was just pressed, so the kitty window
    #: focused right now is the one showing this session -- whatever the attach
    #: mechanism. It used to walk its own process ancestry looking for kitty
    #: instead. For anything attached rather than run in place that walk ends at
    #: tmux, or at Claude's pty host, and so it silently recorded nothing.
    #:
    #: Two guards keep the record honest. No focused kitty window means the
    #: prompt did not come from one -- a message from another agent, the usage
    #: auto-continue -- and nothing is written. A focused window that resolves
    #: to a *different* session means the prompt was injected while you sat
    #: elsewhere, and nothing is written either. Only a window that cannot be
    #: resolved, or one that agrees, is recorded.
    #:
    #: `$1' is the hook's pid. The ancestry walk needed it and this does not; it
    #: stays in the signature because the hook line in
    #: =configFiles/claude-code/settings.json= passes it.
    #: Usage: claude-code-session-register <hook-pid> [payload]
    ##
    local input="${2}"

    if test -z "$input" && ! test -t 0 ; then
        #: Bounded: an inherited pipe that never closes must not wedge the agent's hook.
        input="$(gtimeout 2 cat)" || input=''
    fi
    test -n "$input" || return 0

    local transcript
    transcript="$(ec "$input" | jq -r '.transcript_path // empty' 2>/dev/null)" || return 0
    test -n "$transcript" || return 0

    #: Every early exit below means "not prompted from a kitty window", which is
    #: an ordinary outcome, not an error.
    local sock ls_json win kpid
    sock="$(h-claude-code-session-kitty-socket 2>/dev/null)" || return 0
    ls_json="$(kitty @ --to "${sock}" ls 2>/dev/null)" || return 0
    win="$(h-claude-code-session-focused-window "${ls_json}")" || return 0
    kpid="$(kitty-socket-pid "${sock}")" || return 0

    #: No kitty pid passed, so the resolver does not consult this registry
    #: while we are deciding what to put in it.
    local shown
    if shown="$(h-claude-code-session-of-kitty-window "${ls_json}" "${win}")" ; then
        [[ "${shown}" == "${transcript}" ]] || return 0
    fi

    local dir
    dir="$(h-claude-code-session-registry-dir)" @RET
    mkdir -p "$dir" @TRET

    local key
    key="$(h-claude-code-session-registry-key "${kpid}" "${win}")" @RET
    ec "$transcript" > "${dir}/${key}"
}

function h-claude-code-session-live-list {
    #: Every live Claude Code session, one per line, tab separated: pid, session
    #: id, name, cwd, transcript, tmux session (or `-'), status.
    #:
    #: The work is done by the `live' subcommand of the `claude_session' Go
    #: binary: it runs `claude agents --json' once per config home *in
    #: parallel*, which is the whole cost (~180ms each, and independent), then
    #: reads the tmux field from each session's record and derives the
    #: transcript path. In shell those calls were serial and the resolver spent
    #: most of half a second here; see golang/claude_session/live.go.
    #:
    #: `claude agents' stays the authority on what is live -- that is decided in
    #: Claude Code's daemon and nothing on disk reproduces it. The Go helper
    #: only makes the same call cheaper. [agfi:h-claude-code-session-live-list-sh]
    #: is the identical-output shell fallback for a host where the binary is not
    #: built.
    #:
    #: Set claude_code_session_live_list_cache to reuse one listing across
    #: several lookups; `local' is dynamically scoped, so a caller's assignment
    #: is visible here.
    ##
    if test -n "${claude_code_session_live_list_cache}" ; then
        ec "${claude_code_session_live_list_cache}"
        return 0
    fi

    local -a projects_dirs
    projects_dirs=("${(@f)$(h-claude-code-session-projects-dirs)}") @TRET

    #: The hot path must not pay for a build check every time, so probe with
    #: `command -v' (a real PATH lookup) rather than `isdefined-cmd', whose
    #: answer comes from zsh's command hash -- and the garden's hash is stale
    #: for a binary installed after it started, which would send every call
    #: down the build path. `h-claude-code-session-dep' (which also probes for
    #: `go' and can rebuild) runs only on a genuine first miss.
    if ! command -v claude_session > /dev/null 2>&1 ; then
        h-claude-code-session-dep 2>/dev/null || true
    fi

    if command -v claude_session > /dev/null 2>&1 ; then
        claude_session live "${projects_dirs[@]}" && return 0
    fi

    h-claude-code-session-live-list-sh
}

function h-claude-code-session-live-list-sh {
    #: The pure-shell implementation of [agfi:h-claude-code-session-live-list],
    #: kept as a fallback for when the Go helper is not built. Same columns.
    #:
    #: `claude agents --json' says what is live: it is the supported interface,
    #: it is what the agent view shows, and it costs ~180ms. It is scoped to one
    #: config home, so it runs once per profile -- [agfi:claude-work] keeps a
    #: second one -- and the personal profile is asked with CLAUDE_CONFIG_DIR
    #: *unset*, since Claude Code hashes a set value into its keychain entry
    #: name and would go looking for credentials that are filed under the bare
    #: one. Finished sessions come back with a null pid and are dropped.
    #:
    #: The listing does not carry the tmux location. That comes from Claude
    #: Code's own record of the session, =<config home>/sessions/<pid>.json=,
    #: which is what the listing is built from anyway.
    #:
    #: The transcript path is derived, not read: Claude Code names the projects
    #: directory after the cwd with every non-alphanumeric character turned into
    #: `-' -- slashes, dots, spaces and underscores alike -- under the same
    #: config home the record sits in.
    #:
    #: Set claude_code_session_live_list_cache to reuse one listing across
    #: several lookups; `local' is dynamically scoped, so a caller's assignment
    #: is visible here.
    ##
    ensure-cmd claude jq @RET

    local -a projects_dirs
    projects_dirs=("${(@f)$(h-claude-code-session-projects-dirs)}") @TRET

    #: `st', because `status' is zsh's read-only alias for `$?'.
    local home agents pid sid name cwd st rec tmux_session transcript
    local -a rows
    local row
    for home in "${projects_dirs[@]}" ; do
        #: =~/.claude-work/projects= -> =~/.claude-work=
        home="${home:h}"

        if [[ "${home}" == "${HOME}/.claude" ]] ; then
            agents="$( (unset CLAUDE_CONFIG_DIR ; command claude agents --json) 2>/dev/null )" || continue
        else
            agents="$(CLAUDE_CONFIG_DIR="${home}" command claude agents --json 2>/dev/null)" || continue
        fi

        rows=( ${(f)"$(ec "${agents}" | jq -r '.[] | select(.pid and .sessionId) | [.pid, .sessionId, (.name // "-"), .cwd, (.status // "-")] | @tsv' 2>/dev/null)"} )

        for row in "${rows[@]}" ; do
            IFS=$'\t' read -r pid sid name cwd st <<< "${row}"
            test -n "${pid}" || continue

            tmux_session='-'
            rec="${home}/sessions/${pid}.json"
            if test -e "${rec}" ; then
                tmux_session="$(jq -r '(.tmux // "-") | split(":")[0]' "${rec}" 2>/dev/null)" || tmux_session='-'
            fi

            transcript="${home}/projects/${cwd//[^[:alnum:]]/-}/${sid}.jsonl"

            printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "${pid}" "${sid}" "${name}" "${cwd}" "${transcript}" "${tmux_session:--}" "${st}"
        done
    done
}

function h-claude-code-session-row-transcript {
    #: The transcript of a [agfi:h-claude-code-session-live-list] row, if the
    #: file exists yet. A session that has written nothing is not a target:
    #: there is nothing to show.
    ##
    local -a f
    f=( "${(@ps:\t:)1}" )

    local t="${f[5]}"
    test -n "${t}" && test -e "${t}" || return 1
    ec "${t}"
}

function h-claude-code-session-name-of-transcript {
    #: The live session's name for a transcript, or `-'.
    ##
    local t="${1}"

    local -a live f
    live=( ${(f)"$(h-claude-code-session-live-list)"} )

    local row
    for row in "${live[@]}" ; do
        f=( "${(@ps:\t:)row}" )
        if [[ "${f[5]}" == "${t}" ]] ; then
            ec "${f[3]}"
            return 0
        fi
    done

    ec '-'
}

function h-claude-code-session-tmux-target {
    #: The tmux session a client's command line attaches to: `tmux a -t foo',
    #: `tmux attach -tfoo' and `tmux attach-session -t =foo:2' all give `foo'.
    #: Fails for anything that is not a tmux client naming a session.
    ##
    local cmd="${1}"

    local -a w
    w=( ${(z)cmd} )
    [[ "${w[1]:t}" == tmux ]] || return 1
    [[ "${w[2]}" == (a|at|att|atta|attac|attach|attach-session) ]] || return 1

    local i target=''
    for (( i = 3 ; i <= ${#w} ; i++ )) ; do
        if [[ "${w[i]}" == -t ]] ; then
            target="${w[i+1]}"
            break
        elif [[ "${w[i]}" == -t?* ]] ; then
            target="${w[i]#-t}"
            break
        fi
    done
    test -n "${target}" || return 1

    #: `=name' is tmux's exact-match prefix; `name:win.pane' narrows below the
    #: session, which is all we care about.
    target="${target#=}"
    ec "${target%%:*}"
}

function h-claude-code-session-focused-window {
    #: The id of kitty's focused window, from `kitty @ ls' output. Fails when
    #: no OS window is focused, which is what kitty reports whenever it is not
    #: the frontmost application.
    ##
    local win
    win="$(ec "${1}" | jq -r 'first(.[] | select(.is_focused) | .tabs[] | select(.is_focused) | .windows[] | select(.is_focused) | .id) // empty' 2>/dev/null)"
    test -n "${win}" || return 1
    ec "${win}"
}

function h-claude-code-session-of-kitty-window {
    #: The transcript of the Claude Code session showing in a kitty window, or
    #: failure when that cannot be established.
    #:
    #: A session reaches a kitty window four ways, each leaving a different
    #: trace, so this tries them in order of how sure each one is:
    #:
    #: 1. Claude Code runs in the window itself: a foreground pid of the window
    #:    is a live session's pid.
    #: 2. The window shows a tmux client (`tmux a -t NAME'): NAME is the tmux
    #:    session Claude Code's own record says the session lives in.
    #: 3. The window shows the agent view (`claude agents', `claude attach'):
    #:    the view sets the window title to the attached session's name, at
    #:    times with a status glyph in front. Observed rather than documented,
    #:    so it has to match exactly one live session or it is ignored.
    #: 4. The registry [agfi:claude-code-session-register] keeps as insurance:
    #:    where the session was showing the last time it was prompted.
    #:
    #: The first three read only what exists right now and so cannot go stale;
    #: the fourth can, which is why it comes last. Pass no kitty pid to skip it,
    #: as the hook does when deciding whether to *write* it.
    #: Usage: h-claude-code-session-of-kitty-window <kitty ls json> <window id> [kitty pid]
    ##
    local ls_json="${1}" win="${2}" kpid="${3}"
    test -n "${ls_json}" && test -n "${win}" || return 1

    local title
    title="$(ec "${ls_json}" | jq -r --argjson w "${win}" 'first(.[].tabs[].windows[] | select(.id == $w) | .title) // empty' 2>/dev/null)"

    local -a fg fg_pids fg_cmds
    fg=( ${(f)"$(ec "${ls_json}" | jq -r --argjson w "${win}" '.[].tabs[].windows[] | select(.id == $w) | .foreground_processes[] | "\(.pid)\t\(.cmdline | join(" "))"' 2>/dev/null)"} )
    local l
    for l in "${fg[@]}" ; do
        fg_pids+=("${l%%$'\t'*}")
        fg_cmds+=("${l#*$'\t'}")
    done

    local claude_code_session_live_list_cache="${claude_code_session_live_list_cache:-$(h-claude-code-session-live-list)}"
    local -a live f hits
    live=( ${(f)claude_code_session_live_list_cache} )

    local row
    #: 1. Claude Code in the window itself.
    for row in "${live[@]}" ; do
        if (( ${fg_pids[(Ie)${row%%$'\t'*}]} )) ; then
            h-claude-code-session-row-transcript "${row}" && return 0
        fi
    done

    #: 2. A tmux client.
    local cmd tname
    for cmd in "${fg_cmds[@]}" ; do
        tname="$(h-claude-code-session-tmux-target "${cmd}")" || continue

        hits=()
        for row in "${live[@]}" ; do
            f=( "${(@ps:\t:)row}" )
            [[ "${f[6]}" == "${tname}" ]] && hits+=("${row}")
        done
        #: One tmux session can host several Claude Codes in several panes;
        #: then the window alone does not say which is meant.
        if (( ${#hits} == 1 )) ; then
            h-claude-code-session-row-transcript "${hits[1]}" && return 0
        fi
    done

    #: 3. The agent view, by title.
    local attach_p=n
    local -a w
    for cmd in "${fg_cmds[@]}" ; do
        w=( ${(z)cmd} )
        if [[ "${w[1]:t}" == (claude|claude.exe) ]] || [[ "${w[1]:t}" == (sh|bash|dash|zsh) && "${w[2]:t}" == (claude|claude.exe) ]] ; then
            attach_p=y
            break
        fi
    done
    if [[ "${attach_p}" == y ]] && test -n "${title}" ; then
        #: `✳ LinFine-0' -> `LinFine-0'. The exact title is tried too, for a
        #: name that itself begins with a symbol.
        local stripped="${title}"
        while [[ -n "${stripped}" && "${stripped[1]}" != [[:alnum:]] ]] ; do
            stripped="${stripped[2,-1]}"
        done

        hits=()
        for row in "${live[@]}" ; do
            f=( "${(@ps:\t:)row}" )
            if [[ "${f[3]}" == "${title}" || "${f[3]}" == "${stripped}" ]] ; then
                hits+=("${row}")
            fi
        done
        if (( ${#hits} == 1 )) ; then
            h-claude-code-session-row-transcript "${hits[1]}" && return 0
        fi
    fi

    #: 4. The registry.
    if test -n "${kpid}" ; then
        local key entry t
        if key="$(h-claude-code-session-registry-key "${kpid}" "${win}")" ; then
            entry="$(h-claude-code-session-registry-dir)/${key}"
            if test -e "${entry}" ; then
                t="$(<"${entry}")"
                if test -n "${t}" && test -e "${t}" ; then
                    ec "${t}"
                    return 0
                fi
            fi
        fi
    fi

    return 1
}

function h-claude-code-session-pick-overlay {
    #: Opens the session picker as a kitty overlay over the active window, for
    #: when the window itself does not say which session it shows.
    #:
    #: It runs as a small script, =zshlang/wrappers/claude-code-session-pick.zsh=,
    #: because fzf needs a terminal and nothing on this side has one: the hotkey
    #: runs in the background and the garden's shells are not interactive. The
    #: script gets its rows and hands its choice back through the garden.
    #:
    #: kitty's own environment has the bare macOS PATH, so ours is passed in.
    #: Usage: h-claude-code-session-pick-overlay <kitty socket>
    ##
    local sock="${1}"
    assert-args sock @RET

    local wrappers="${NIGHTDIR:-${HOME}/scripts}/zshlang/wrappers"

    kitty @ --to "${sock}" launch --type=overlay --title 'Claude Code sessions' \
        --env "PATH=${PATH}" \
        --env "NIGHTDIR=${NIGHTDIR:-${HOME}/scripts}" \
        --env "FZF_DEFAULT_OPTS=${FZF_DEFAULT_OPTS}" \
        "${wrappers}/zshplain.dash" "${wrappers}/claude-code-session-pick.zsh" >/dev/null
}

function h-claude-code-session-kitty-socket {
    #: The kitty instance to talk to, as `unix:<path>'.
    #:
    #: A thin wrapper over [agfi:kitty-socket-get], which owns everything about
    #: where the socket lives and which kitty owns it. It used to demand
    #: *exactly one* match from the glob, so a single socket left behind by a
    #: crashed kitty was enough to break this; the shared resolver filters by
    #: live pid instead, and so needs no such rule.
    #:
    #: On failure the reason is on stderr, which
    #: [agfi:h-claude-code-view-session-focused] captures verbatim -- the reason
    #: is the whole point, since "no socket" and "kitty is running but its
    #: socket is gone, restart it" want completely different responses.
    #:
    #: `claude_code_session_kitty_socket_glob' stays as the documented override
    #: for the tests; zsh scopes it dynamically, so assigning it here is
    #: visible to the resolver.
    ##
    local kitty_sockets_list_glob="${claude_code_session_kitty_socket_glob:-${kitty_sockets_list_glob}}"

    kitty-socket-get
}

function h-claude-code-session-lost {
    #: The hotkey runs detached, so stderr goes nowhere a person will look.
    ##
    ecerr "claude-code-view-session-focused: ${1}"
    silence notif "Claude session: ${1}"
    return 1
}

function claude-code-view-session-focused {
    #: Opens the Claude Code session running in the focused kitty window as an
    #: org file in emacs. Bound to a kitty hotkey; the window -> session
    #: mapping comes from [agfi:claude-code-session-register].
    #:
    #: Always succeeds. Every way this comes up empty -- no session in this
    #: window, kitty not answering -- is a message for a person, and it has
    #: already been delivered as a notification by the time we get here. A
    #: non-zero return would only add BrishGarden's failed-command bell on top
    #: of it, for something that is not a failure.
    ##
    h-claude-code-view-session-focused || true
}

function h-claude-code-view-session-focused {
    #: The body of [agfi:claude-code-view-session-focused], split out so the
    #: hotkey can swallow the exit code without swallowing the reason.
    #:
    #: The focused window goes through [agfi:h-claude-code-session-of-kitty-window]
    #: and whatever that finds is opened. When it finds nothing, the answer is a
    #: picker over every live session, as an overlay in that same window, rather
    #: than a notification: "which one did you mean" is a question, not a
    #: failure. Everything that *is* a failure reports through
    #: [agfi:h-claude-code-session-lost].
    ##
    if ! ensure-cmd kitty jq ; then
        h-claude-code-session-lost "kitty or jq is not installed"
        return 1
    fi

    #: stdout and stderr together: on success this is the socket, on failure it
    #: is the reason, and a subshell cannot hand a variable back to us.
    local sock
    sock="$(h-claude-code-session-kitty-socket 2>&1)"
    if [[ "${sock}" != unix:* ]] ; then
        h-claude-code-session-lost "${${sock#kitty-socket-get: }:-could not find kitty's socket}"
        return 1
    fi

    local ls_json
    if ! ls_json="$(kitty @ --to "${sock}" ls)" ; then
        h-claude-code-session-lost "kitty did not answer on ${sock}"
        return 1
    fi

    local win
    if ! win="$(h-claude-code-session-focused-window "${ls_json}")" ; then
        h-claude-code-session-lost "could not identify the focused kitty window"
        return 1
    fi

    local kpid
    kpid="$(kitty-socket-pid "${sock}")" @RET

    local transcript
    if transcript="$(h-claude-code-session-of-kitty-window "${ls_json}" "${win}" "${kpid}")" ; then
        claude-code-view-session "${transcript}"
        return $?
    fi

    if ! h-claude-code-session-pick-overlay "${sock}" ; then
        h-claude-code-session-lost "could not tell which session this window shows, and could not open the picker either"
        return 1
    fi
}
##
function claude-session-selftest {
    #: Runs the renderer's Go tests, then checks its parallel pandoc path
    #: against a single pandoc run over every local session transcript.
    ##
    ensure-cmd go pandoc @RET

    local dir="${NIGHTDIR}/golang/claude_session"
    #: Every profile's transcripts, one run each: the parity check takes a
    #: single directory, and picking just one of them would quietly shrink the
    #: corpus to whichever sorted first.
    local -a corpus_dirs
    corpus_dirs=("${(@f)$(h-claude-code-session-projects-dirs)}") @TRET

    pushf "${dir}" && {
        assert go test -count=1 ./... @RET

        local corpus
        for corpus in "${corpus_dirs[@]}" ; do
            ecgray "$0: parity over ${corpus/#${HOME}/~}"
            CLAUDE_SESSION_CORPUS="${corpus}" assert go test -count=1 -v -run Parity ./... @RET
        done
    } always { popf }
}
##
function h-claude-code-session-live-pairs {
    #: `<kitty-window-id> <TAB> <transcript> <TAB> <name>' for every kitty
    #: window showing a Claude Code session, one per line.
    #:
    #: Each window goes through [agfi:h-claude-code-session-of-kitty-window], so
    #: this sees sessions attached through tmux or the agent view, not only ones
    #: that registered themselves. One `claude agents' listing serves every
    #: window, through the cache the resolver honours.
    ##
    ensure-cmd kitty jq @RET

    local sock
    sock="$(h-claude-code-session-kitty-socket)" @RET

    local kpid
    kpid="$(kitty-socket-pid "${sock}")" @RET

    local ls_json
    ls_json="$(kitty @ --to "${sock}" ls)" @RET

    local claude_code_session_live_list_cache="${claude_code_session_live_list_cache:-$(h-claude-code-session-live-list)}"

    local -a ids
    ids=("${(@f)$(ec "${ls_json}" | jq -r '.[].tabs[].windows[].id')}") @TRET

    local id transcript
    for id in "${ids[@]}" ; do
        test -n "${id}" || continue

        transcript="$(h-claude-code-session-of-kitty-window "${ls_json}" "${id}" "${kpid}")" || continue

        printf '%s\t%s\t%s\n' "${id}" "${transcript}" "$(h-claude-code-session-name-of-transcript "${transcript}")"
    done
}

function h-claude-code-session-all-pairs {
    #: [agfi:h-claude-code-session-live-pairs] over every live session: the
    #: window column reads `-' for a session no kitty window is showing.
    #: Windows first, so a session showing in one is listed under it.
    ##
    local claude_code_session_live_list_cache="${claude_code_session_live_list_cache:-$(h-claude-code-session-live-list)}"

    local in_windows
    in_windows="$(h-claude-code-session-live-pairs 2>/dev/null)" || in_windows=''
    test -n "${in_windows}" && ec "${in_windows}"

    local -a live f
    live=( ${(f)claude_code_session_live_list_cache} )

    local row t
    for row in "${live[@]}" ; do
        t="$(h-claude-code-session-row-transcript "${row}")" || continue
        [[ "${in_windows}" == *$'\t'"${t}"$'\t'* ]] && continue

        f=( "${(@ps:\t:)row}" )
        printf -- '-\t%s\t%s\n' "${t}" "${f[3]}"
    done
}

function h-claude-code-session-pick-rows {
    #: [agfi:h-claude-code-session-live-rows] over every live session, showing
    #: in a kitty window or not. What the overlay picker offers.
    ##
    claude_code_session_live_rows_scope=all h-claude-code-session-live-rows
}

function h-claude-code-session-live-rows {
    #: The rows [agfi:claude-code-session-live-fz] offers, tab separated:
    #: window id, transcript, label (`w<id> <name>`, or the bare name for a
    #: session in no window), profile, last activity, relative path, snippet.
    #: The first two are for the caller, the rest for the person choosing.
    ##
    #: `windows' (default): sessions showing in a kitty window, from
    #: [agfi:h-claude-code-session-live-pairs]. `all': every live session, from
    #: [agfi:h-claude-code-session-all-pairs], `-' in the window column when
    #: none shows it.
    local scope="${claude_code_session_live_rows_scope:-windows}"

    local pairs
    if [[ "${scope}" == all ]] ; then
        pairs="$(h-claude-code-session-all-pairs)" @RET
    else
        pairs="$(h-claude-code-session-live-pairs)" @RET
    fi
    if test -z "${pairs}" ; then
        ecerr "$0: no live Claude Code session with a transcript to show"
        return 1
    fi

    local -a projects_dirs
    projects_dirs=("${(@f)$(h-claude-code-session-projects-dirs)}") @TRET

    h-claude-code-session-dep @RET

    #: One `list` over every root and then a join, rather than a metadata call
    #: per row: `list` does the whole corpus in ~45ms, while `claude_session
    #: name` alone costs ~230ms on a large transcript. Anything the join misses
    #: still gets a row, just a barer one.
    local meta
    meta="$(claude_session list "${projects_dirs[@]}")" || meta=''

    ec "${pairs}" |
        gawk -F'\t' -v OFS='\t' '
            NR == FNR { when[$2] = $3 ; rel[$2] = $4 ; snip[$2] = $5 ; next }
            {
                path = $2

                #: The profile is the config home the transcript sits under --
                #: .claude, .claude-work -- which is the same label
                #: `claude_session list` puts on its own relative paths.
                profile = path
                sub(/\/projects\/.*$/, "", profile)
                sub(/^.*\//, "", profile)

                label = ($1 == "-") ? $3 : ("w" $1 "  " $3)

                print $1, path, label, profile, \
                    (when[path] ? when[path] : "?"), \
                    (rel[path] ? rel[path] : path), \
                    snip[path]
            }
        ' <(ec "${meta}") -
}

function h-claude-code-session-preview {
    #: The fzf preview body for [agfi:claude-code-session-live-fz]: what this
    #: session is called, when it last moved, and what was last asked of it.
    #:
    #: A tail scan rather than `claude_session render`, because a preview has to
    #: be instant and rendering a 46MB transcript takes eight seconds. Claude
    #: Code writes its own `ai-title` and `last-prompt` records, so the two
    #: things worth previewing are already sitting there in plain form.
    ##
    local transcript="${1}"
    assert-args transcript @RET

    local bytes="${claude_code_session_preview_bytes:-400000}"

    #: `tail -n +2` drops the partial line a byte-tail necessarily starts on,
    #: and `fromjson?` drops anything else that does not parse. The timestamp
    #: branch comes last: the title and prompt records carry no timestamp, and
    #: putting it first would swallow them.
    local tagged
    tagged="$(command tail -c "${bytes}" "${transcript}" 2>/dev/null |
        command tail -n +2 |
        jq -Rr 'fromjson?
            | if   .type == "ai-title"    then "T\t" + (.aiTitle    | tostring)
              elif .type == "last-prompt" then "P\t" + (.lastPrompt | tostring)
              elif .timestamp             then "S\t" + (.timestamp  | tostring)
              else empty end' 2>/dev/null)" || tagged=''

    #: `cut -f2-` drops the tag the scan above put on, and keeps a value that
    #: itself contains tabs intact.
    local title prompt stamp
    title="$(ec "${tagged}"  | command grep $'^T\t' | command tail -n 1 | command cut -f2-)"
    prompt="$(ec "${tagged}" | command grep $'^P\t' | command tail -n 1 | command cut -f2-)"
    stamp="$(ec "${tagged}"  | command grep $'^S\t' | command tail -n 1 | command cut -f2-)"

    ec "${title:-Claude Code session ${transcript:t:r}}"
    ec
    ec "session:  ${transcript:t:r}"
    ec "modified: ${stamp:-unknown}"
    ec
    ec 'last prompt:'
    ec "${prompt:-(none in the scanned tail)}"
}

function claude-code-session-live-fz {
    #: Picks among the Claude Code sessions currently live in a kitty window,
    #: and prints `<kitty-window-id> <TAB> <transcript path>` for each choice.
    #:
    #: Multi-select by default, since the callers want to act on several tabs at
    #: once; set claude_code_session_live_fz_no_multi_p=y for one.
    #: claude_code_session_live_fz_extra_rows prepends synthetic choices, which
    #: is how [agfi:h-claude-code-usage-type-continue-target-fz] offers
    #: "frontmost" alongside the real sessions.
    ##
    local rows
    rows="$(h-claude-code-session-live-rows)" @RET

    ensure-array claude_code_session_live_fz_extra_rows
    if (( ${#claude_code_session_live_fz_extra_rows} )) ; then
        rows="${(F)claude_code_session_live_fz_extra_rows}"$'\n'"${rows}"
    fi

    ensure-array claude_code_session_live_fz_opts
    local fz_opts=("${claude_code_session_live_fz_opts[@]}")

    local multi_opt='--multi'
    if bool "${claude_code_session_live_fz_no_multi_p:-n}" ; then
        multi_opt='--no-multi'
    fi

    #: Through the garden, the established way to reach a zsh function from
    #: fzf's own shell (cf. the `execute-silent` binds in [agfi:h-grep-output-to-fz]).
    #: Remember `brishz-restart` after editing the preview, or fzf keeps calling
    #: whatever the garden loaded at startup.
    local selected
    selected="$(ec "${rows}" |
        fz_no_preview=y fz \
            --delimiter=$'\t' --with-nth='3..' \
            "${multi_opt}" \
            --preview 'brishzq.zsh h-claude-code-session-preview {2}' \
            --preview-window 'down,60%,wrap' \
            "${fz_opts[@]}")" @RET

    test -n "${selected}" || return 1

    #: The display columns were only ever for the person choosing.
    ec "${selected}" | command cut -f1,2
}
##
#: Resuming a session under another profile
##
function h-claude-code-profile-config-home {
    #: The config home of a registered profile: the directory holding its
    #: `projects/', `file-history/' and `sessions/'. The default profile has no
    #: CLAUDE_CONFIG_DIR and lives at =~/.claude= (its `.claude.json' is at
    #: =~/.claude.json=, which nothing here needs). Registry:
    #: `claude_code_profiles' in claude.zsh, see [agfi:claude-work].
    ##
    local profile="${1}"
    assert-args profile @RET
    h-claude-code-profile-assert "${profile}" @RET

    local dir="${claude_code_profiles[${profile}]}"
    ec "${dir:-${HOME}/.claude}"
}

function h-claude-code-session-profile-of {
    #: The registered profile whose config home holds the given transcript.
    #: Longest matching home wins, so a profile nested under another's home
    #: would still resolve correctly.
    ##
    local transcript="${1}"
    assert-args transcript @RET

    local transcript_abs="${transcript:a}"
    local profile home best='' best_len=0
    for profile in "${claude_code_profile_order[@]}" ; do
        home="$(h-claude-code-profile-config-home "${profile}")" @RET
        home="${home:a}"
        if [[ "${transcript_abs}" == "${home}"/* ]] && (( ${#home} > best_len )) ; then
            best="${profile}"
            best_len="${#home}"
        fi
    done

    if test -z "${best}" ; then
        ecerr "$0: transcript is under no registered profile: ${transcript}"
        return 1
    fi

    ec "${best}"
}

function h-claude-code-session-resolve {
    #: A transcript path from either a path or a session uuid (a unique prefix
    #: of one will do). A uuid is looked up under every profile's projects
    #: directory, [agfi:h-claude-code-session-projects-dirs], since the caller
    #: usually does not know which seat a session was started on. Several
    #: matches are an error rather than a guess: the same uuid in two profiles
    #: is exactly what [agfi:claude-code-session-import] exists to avoid.
    ##
    local input="${1}"
    assert-args input @RET

    if [[ "${input}" == *.jsonl ]] || [[ "${input}" == */* ]] ; then
        if ! test -e "${input}" ; then
            ecerr "$0: transcript does not exist: ${input}"
            return 1
        fi
        ec "${input:a}"
        return 0
    fi

    local -a projects_dirs
    projects_dirs=("${(@f)$(h-claude-code-session-projects-dirs)}") @TRET

    local -a hits
    local d
    for d in "${projects_dirs[@]}" ; do
        hits+=( "${d}"/*/"${input}"*.jsonl(N) )
    done

    if (( ${#hits} == 0 )) ; then
        ecerr "$0: no session matches '${input}' under: ${(j:, :)projects_dirs}"
        return 1
    elif (( ${#hits} > 1 )) ; then
        ecerr "$0: '${input}' is ambiguous:"
        ecerr "  ${(pj:\n  :)hits}"
        return 1
    fi

    ec "${hits[1]}"
}

function h-claude-code-session-live-row-of {
    #: The [agfi:h-claude-code-session-live-list] row whose transcript is the
    #: given file, or failure when no running session is writing it.
    ##
    local transcript="${1:a}"
    assert-args transcript @RET

    local claude_code_session_live_list_cache="${claude_code_session_live_list_cache:-$(h-claude-code-session-live-list)}"

    local row t
    for row in "${(f)claude_code_session_live_list_cache}" ; do
        t="$(h-claude-code-session-row-transcript "${row}")" || continue
        if [[ "${t:a}" == "${transcript}" ]] ; then
            ec "${row}"
            return 0
        fi
    done

    return 1
}

function claude-code-session-import {
    #: Forks a session into another profile: copies its transcript and the
    #: state keyed by its uuid into the target profile's config home under a
    #: *new* uuid, and prints the new transcript's path. The source is never
    #: modified. The point is to continue a conversation on a different
    #: account -- the work seat has hit its usage limit, say -- since
    #: `claude --resume' only ever searches the active profile.
    #:
    #: A new uuid, not a same-uuid copy: a uuid present in two profiles makes
    #: the agent-view title match in [agfi:h-claude-code-session-of-kitty-window]
    #: and Claude Code's own `--resume <name>' ambiguous, and resuming the
    #: stale copy later would fork it silently. The uuid appears in the
    #: transcript only as `sessionId' (one distinct value, ~one per line), in
    #: the subagent transcripts, and in directory names, so it is rewritten in
    #: place in the copies. The fork gets the source's name plus a suffix
    #: (`claude_code_session_import_name_suffix', a printf format taking the
    #: target profile; `⑂' is the glyph Claude Code appends on
    #: `--fork-session'), written as both an `agent-name' line, which
    #: `claude_session name' prefers, and a `custom-title' line, which is what
    #: `/rename' writes and stops Claude Code re-titling.
    #:
    #: Copied: `<uuid>.jsonl', `<uuid>/' (subagents, tool results),
    #: `file-history/<uuid>/' (for `/rewind'). Not copied: `plans/' (the file
    #: name is not derivable from the transcript), the per-project auto-memory
    #: and the prompt history, which are per profile by design.
    #:
    #: Instruction files are not part of a transcript: Claude Code injects
    #: them from the active config dir at launch, so the fork runs under the
    #: *target* profile's assembled CLAUDE.md. Into a non-default profile that
    #: means the whole history enters that profile's store before its overlay
    #: -- the privacy guardrails of [agfi:claude-work] -- ever applies, so it
    #: asks first unless claude_code_session_import_yes_p=y.
    #:
    #: Refuses a live source unless claude_code_session_import_force_p=y: the
    #: running process keeps appending to it, so the fork would be stale at
    #: once. claude_code_session_import_remove_source_p=y trashes the source
    #: afterwards, so the session leaves the source profile's picker; off by
    #: default, since with distinct uuids the leftover is clutter, not a
    #: hazard.
    #:
    #: Usage: claude-code-session-import <transcript|uuid> <to-profile>
    ##
    local remove_source_p="${claude_code_session_import_remove_source_p:-n}"
    local force_p="${claude_code_session_import_force_p:-n}"
    local yes_p="${claude_code_session_import_yes_p:-n}"
    local name_suffix="${claude_code_session_import_name_suffix:- ⑂ %s}"

    local to_profile="${2}"
    assert-args to_profile @RET
    h-claude-code-profile-assert "${to_profile}" @RET
    ensure-cmd gcp perl jq uuidgen @RET
    h-claude-code-session-dep @RET

    local source
    source="$(h-claude-code-session-resolve "${1}")" @RET

    local from_profile
    from_profile="$(h-claude-code-session-profile-of "${source}")" @RET
    if [[ "${from_profile}" == "${to_profile}" ]] ; then
        ecerr "$0: session already belongs to profile '${to_profile}': ${source}"
        return 1
    fi

    local from_home to_home
    from_home="$(h-claude-code-profile-config-home "${from_profile}")" @RET
    to_home="$(h-claude-code-profile-config-home "${to_profile}")" @RET

    #: =<home>/projects/<encoded cwd>/<uuid>.jsonl=
    local enc="${source:h:t}"
    local old="${source:t:r}"
    local new
    new="${$(uuidgen):l}" @TRET

    local live_row
    if live_row="$(h-claude-code-session-live-row-of "${source}")" ; then
        local -a f
        f=( "${(@ps:\t:)live_row}" )
        if bool "${force_p}" ; then
            ecerr "$0: warning: source session is live (pid ${f[1]}, tmux ${f[6]}); the fork stops where it is now"
        else
            ecerr "$0: source session is live (pid ${f[1]}, tmux ${f[6]}). Quit it first so the fork is complete, or set claude_code_session_import_force_p=y."
            return 1
        fi
    fi

    if [[ "${to_profile}" != default ]] && ! bool "${yes_p}" ; then
        ecerr "$0: the whole conversation so far will be stored under profile '${to_profile}' (${to_home}); its own instruction files, including any privacy guardrails, only apply from here on."
        if ! { : </dev/tty ; } 2>/dev/null ; then
            ecerr "$0: no terminal to confirm on; set claude_code_session_import_yes_p=y to proceed"
            return 1
        fi
        if ! ask "Import into '${to_profile}' anyway?" n ; then
            ecerr "$0: aborted; nothing was written"
            return 1
        fi
    fi

    local target_dir="${to_home}/projects/${enc}"
    local target="${target_dir}/${new}.jsonl"
    assert mkdir -p "${target_dir}" @RET
    assert gcp --archive -- "${source}" "${target}" @RET

    local source_side="${source:r}"
    if test -d "${source_side}" ; then
        assert gcp --archive -- "${source_side}" "${target_dir}/${new}" @RET
    fi

    local source_fh="${from_home}/file-history/${old}"
    if test -d "${source_fh}" ; then
        assert mkdir -p "${to_home}/file-history" @RET
        assert gcp --archive -- "${source_fh}" "${to_home}/file-history/${new}" @RET
    fi

    #: Only the copies are touched. A uuid is `[0-9a-f-]', so nothing in it
    #: needs escaping, but \Q..\E costs nothing.
    local -a rewrite_files
    rewrite_files=( "${target}" "${target_dir}/${new}"/subagents/*.jsonl(N) )
    assert perl -pi -e "s/\\Q${old}\\E/${new}/g" -- "${rewrite_files[@]}" @RET

    local name
    name="$(claude_session name "${source}")" @RET
    if test -z "${name}" ; then
        name="${old[1,8]}"
    fi
    #: A fork of a fork keeps one suffix, not a trail of them.
    local p sfx
    for p in "${claude_code_profile_order[@]}" ; do
        sfx="$(printf -- "${name_suffix}" "${p}")"
        name="${name%"${sfx}"}"
    done
    local new_name
    new_name="${name}$(printf -- "${name_suffix}" "${to_profile}")" @TRET

    #: The transcript is a jsonl; make sure the new lines start on their own.
    if [[ "$(tail -c 1 "${target}")" != $'\n' ]] ; then
        ec >> "${target}"
    fi
    jq --compact-output --null-input \
        --arg name "${new_name}" --arg sid "${new}" \
        '{type: "agent-name", agentName: $name, sessionId: $sid},
         {type: "custom-title", customTitle: $name, sessionId: $sid}' >> "${target}" @RET

    if bool "${remove_source_p}" ; then
        #: `trs' narrates on stdout; keep stdout for the path.
        {
            trs "${source}" @RET
            if test -d "${source_side}" ; then
                trs "${source_side}" @RET
            fi
            if test -d "${source_fh}" ; then
                trs "${source_fh}" @RET
            fi
        } 1>&2
    fi

    ecerr "$0: ${from_profile} -> ${to_profile}: '${new_name}' (${new})"
    ec "${target}"
}

function claude-code-session-resume {
    #: Resumes a Claude Code session under a profile: the one that owns it, or
    #: another one, in which case [agfi:claude-code-session-import] forks it
    #: there first. Starts that profile's launcher from
    #: `claude_code_profile_launchers' with `--resume <uuid>'. The
    #: non-interactive counterpart of [agfi:claude-code-session-resume-fz].
    #:
    #: Anything after the second argument goes to the launcher. Tools run in
    #: the current directory, not the one the session was started in, so this
    #: warns when the two differ.
    #:
    #: Usage: claude-code-session-resume <transcript|uuid> [to-profile] [claude args...]
    ##
    local session="${1}"
    local to_profile="${2}"
    local -a extra
    extra=("${@[3,-1]}")
    assert-args session @RET

    local source
    source="$(h-claude-code-session-resolve "${session}")" @RET

    local from_profile
    from_profile="$(h-claude-code-session-profile-of "${source}")" @RET
    to_profile="${to_profile:-${from_profile}}"
    h-claude-code-profile-assert "${to_profile}" @RET

    local launcher="${claude_code_profile_launchers[${to_profile}]}"
    if test -z "${launcher}" ; then
        ecerr "$0: no launcher registered for profile '${to_profile}' in claude_code_profile_launchers"
        return 1
    fi

    local transcript="${source}"
    if [[ "${to_profile}" != "${from_profile}" ]] ; then
        transcript="$(claude-code-session-import "${source}" "${to_profile}")" @RET
    fi

    local enc_pwd="${PWD//[^[:alnum:]]/-}"
    if [[ "${transcript:h:t}" != "${enc_pwd}" ]] ; then
        ecerr "$0: warning: session was started in another directory (${transcript:h:t}); tools will run in ${PWD}"
    fi

    "${launcher}" --resume "${transcript:t:r}" "${extra[@]}"
}
aliasfn claude-resume claude-code-session-resume

function claude-code-session-resume-fz {
    #: Picks a session with [agfi:h-claude-code-session-select-fz] -- every
    #: profile's copy of the current project, rows labelled by profile -- and
    #: hands it to [agfi:claude-code-session-resume].
    #: claude_code_session_resume_all_p=y widens the choice to every project.
    #:
    #: Usage: claude-code-session-resume-fz [to-profile] [claude args...]
    ##
    local all_p="${claude_code_session_resume_all_p:-n}"

    local claude_code_view_session_fz_scope='project'
    if bool "${all_p}" ; then
        claude_code_view_session_fz_scope='all'
    fi

    local source
    source="$(h-claude-code-session-select-fz)" @RET

    claude-code-session-resume "${source}" "$@"
}
aliasfn claude-resume-fz claude-code-session-resume-fz

function claude-resume-personal {
    #: [agfi:claude-code-session-resume] into the default profile: continue a
    #: work session on the personal account.
    #: Usage: claude-resume-personal <transcript|uuid> [claude args...]
    ##
    claude-code-session-resume "${1}" default "${@[2,-1]}"
}
aliasfn claude-resume-personal-fz claude-code-session-resume-fz default

function claude-resume-work {
    #: [agfi:claude-code-session-resume] into the work profile.
    #: Usage: claude-resume-work <transcript|uuid> [claude args...]
    ##
    claude-code-session-resume "${1}" work "${@[2,-1]}"
}
aliasfn claude-resume-work-fz claude-code-session-resume-fz work
##
