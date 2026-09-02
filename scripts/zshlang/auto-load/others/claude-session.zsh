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
        local project_dir_name="${${PWD//\//-}//./-}"
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
    #: Where [agfi:claude-code-session-register] records which Claude Code
    #: session is running in which kitty window.
    ##
    ec "${claude_code_session_registry_dir:-${HOME}/tmp/claude-code-sessions}"
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

function h-claude-code-session-key-of-pid {
    #: The registry key for the kitty window a process is running in, found by
    #: walking the process's ancestors against what kitty reports as each
    #: window's foreground processes.
    #:
    #: `KITTY_WINDOW_ID` would be far simpler, but Claude Code does not pass it
    #: down: its sessions come out carrying `KITTY_PID` and `KITTY_LISTEN_ON`
    #: but no window id, and the detached ones carry nothing at all. The
    #: interactive sessions do show up in `kitty @ ls` though, so ask kitty.
    #: Usage: h-claude-code-session-key-of-pid <pid>
    ##
    local pid="${1}"
    test -n "${pid}" || return 1

    local sock
    sock="$(h-claude-code-session-kitty-socket)" || return 1

    local fg
    fg="$(kitty @ --to "${sock}" ls | jq -r '.[] | .tabs[] | .windows[] | . as $w | .foreground_processes[] | "\(.pid) \($w.id)"')" || return 1

    local -A window_of
    local line
    for line in ${(f)fg} ; do
        window_of[${line%% *}]="${line##* }"
    done

    #: Bounded: a cycle in the ancestry would otherwise spin forever.
    local -i hops=0
    while (( hops < 32 )) && test -n "${pid}" && [[ "${pid}" != 0 ]] ; do
        if test -n "${window_of[${pid}]}" ; then
            #: `unix:/Users/evar/tmp/.kitty-527` -> `527`; greedy, so dashes in
            #: the path do not matter.
            h-claude-code-session-registry-key "${sock##*-}" "${window_of[${pid}]}"
            return $?
        fi

        pid="$(ps -o ppid= -p "${pid}" 2>/dev/null | tr -d ' ')"
        (( hops++ ))
    done

    return 1
}

function claude-code-session-register {
    #: Records the calling Claude Code session's transcript path, keyed by the
    #: kitty window it runs in, so [agfi:claude-code-view-session-focused] can
    #: find it again. For Claude Code's `SessionStart` and `UserPromptSubmit`
    #: hooks; the payload is JSON, taken from `$2` or from stdin.
    #:
    #: The hook passes its own pid because brish posts only the command and its
    #: stdin to the garden -- a hook's environment does not survive the trip,
    #: and process ancestry is readable from anywhere anyway.
    #: Usage: claude-code-session-register <hook-pid> [payload]
    ##
    local pid="${1}" input="${2}"

    #: No kitty window above us: a detached agent, a server, an ssh session.
    #: Nothing there could press the hotkey either, so this is not an error.
    local key
    key="$(h-claude-code-session-key-of-pid "${pid}")" || return 0

    if test -z "$input" && ! test -t 0 ; then
        #: Bounded: an inherited pipe that never closes must not wedge the agent's hook.
        input="$(gtimeout 2 cat)" || input=''
    fi
    test -n "$input" || return 0

    local transcript
    transcript="$(ec "$input" | jq -r '.transcript_path // empty' 2>/dev/null)" || return 0
    test -n "$transcript" || return 0

    local dir
    dir="$(h-claude-code-session-registry-dir)" @RET
    mkdir -p "$dir" @TRET

    ec "$transcript" > "${dir}/${key}"
}

function claude-code-session-unregister {
    #: Drops a kitty window's registry entry, for Claude Code's `SessionEnd`
    #: hook. Entries already become unreachable when kitty restarts (the key
    #: carries kitty's pid), so this is hygiene rather than correctness.
    #: Usage: claude-code-session-unregister <hook-pid>
    #:
    #: This is a rather useless function, I removed its hook. It strictly makes things worse.
    ##
    local pid="${1}"

    local key
    key="$(h-claude-code-session-key-of-pid "${pid}")" || return 0

    local dir
    dir="$(h-claude-code-session-registry-dir)" @RET

    command rm -f "${dir}/${key}"
}

function h-claude-code-session-kitty-socket {
    #: The kitty instance to talk to.
    #:
    #: `KITTY_LISTEN_ON` is only trusted if it still points at a live socket:
    #: brish's shells outlive kitty, so the garden holds whatever value was in
    #: the environment the day it was started, which goes stale the moment
    #: kitty restarts. The glob is both the fallback and the common path.
    ##
    if [[ "${KITTY_LISTEN_ON}" == unix:* ]] && test -e "${KITTY_LISTEN_ON#unix:}" ; then
        ec "${KITTY_LISTEN_ON}"
        return 0
    fi

    #: Matches `listen_on` in =configFiles/kitty/kitty.conf=; kitty appends its pid.
    local socks=( ${~${claude_code_session_kitty_socket_glob:-${HOME}/tmp/.kitty-*}}(N) )
    if (( ${#socks} != 1 )) ; then
        ecerr "$0: expected exactly one kitty socket, found ${#socks}"
        return 1
    fi

    ec "unix:${socks[1]}"
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
    #: hotkey can swallow the exit code without swallowing the reason. Every
    #: exit reports through [agfi:h-claude-code-session-lost].
    ##
    if ! ensure-cmd kitty jq ; then
        h-claude-code-session-lost "kitty or jq is not installed"
        return 1
    fi

    local sock
    if ! sock="$(h-claude-code-session-kitty-socket)" ; then
        h-claude-code-session-lost "could not find kitty's socket"
        return 1
    fi

    local ls_json
    if ! ls_json="$(kitty @ --to "${sock}" ls)" ; then
        h-claude-code-session-lost "kitty did not answer on ${sock}"
        return 1
    fi

    local win
    win="$(ec "${ls_json}" | jq -r 'first(.[] | select(.is_focused) | .tabs[] | select(.is_focused) | .windows[] | select(.is_focused) | .id) // empty')"

    local key
    if ! key="$(h-claude-code-session-registry-key "${sock##*-}" "${win}")" ; then
        h-claude-code-session-lost "could not identify the focused kitty window"
        return 1
    fi

    local entry
    entry="$(h-claude-code-session-registry-dir)/${key}"
    if ! test -e "${entry}" ; then
        h-claude-code-session-lost "no Claude Code session registered for this window"
        return 1
    fi

    local session_file
    session_file="$(cat "${entry}")"
    if ! test -e "${session_file}" ; then
        #: Usually a session that has not been written to yet: `SessionStart`
        #: reports the path before Claude Code creates the file, or even the
        #: project directory. `UserPromptSubmit` re-registers, so this clears
        #: itself as soon as there is anything worth reading.
        h-claude-code-session-lost "this session has no transcript on disk yet: ${session_file}"
        return 1
    fi

    claude-code-view-session "${session_file}"
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
