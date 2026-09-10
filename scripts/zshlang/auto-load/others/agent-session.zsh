##
#: Reading, picking and resuming coding-agent sessions -- Claude Code, Codex,
#: Antigravity -- with one set of helpers. Everything here takes a transcript
#: path and infers the agent from it ([agfi:h-agent-session-agent-of]); the
#: agent-specific parts (where transcripts live, how to resume, what a hook
#: payload says) are the adapter verbs of =claude-session.zsh=,
#: =codex-session.zsh= and =agy-session.zsh=, reached through
#: [agfi:h-agent-session-call]. Rendering and scanning are done by the Go
#: binary =golang/agent_session=; see its =readme.org= and
#: =docs/agent-sessions.md=.
#:
#: Knobs are `agent_session_*'; the older `claude_code_*' names are still read
#: where they existed, so an override in `personal/' keeps working.
##
function h-agent-session-dep {
    #: Ensures the renderer is built and on PATH, building it on first use.
    #:
    #: The probe is `whence -p', a PATH search that stops at the first hit, and
    #: not [agfi:isdefined-cmd]. Reading `$commands' fills the whole command
    #: hash, and in a garden shell -- which forks per call, so the hash is
    #: never warm -- that measured 65ms every time, against 6ms for a lookup
    #: that stops early. This guard is on the path of every render, name, live
    #: listing and preview, so it is worth the odd spelling.
    ##
    if whence -p agent_session > /dev/null 2>&1 ; then
        return 0
    fi

    ensure-cmd go @RET
    ensure-dep1 agent_session go-install-local "${NIGHTDIR}/golang/agent_session" @RET
}

function h-agent-session-name {
    #: The session's own name -- whatever the agent calls it, else its id for
    #: sessions predating names. Sanitized for use as a filename.
    #:
    #: The Go side answers this, unless the agent defines a `name' verb of its
    #: own: Antigravity does, because its titles live in a SQLite database that
    #: the stdlib-only renderer cannot read, and the picker's column has to
    #: agree with the tmux session name, which comes from there.
    ##
    local input="${1}"
    assert-args input @RET

    local agent
    agent="$(h-agent-session-agent-of "${input}")" @RET

    local name ret
    name="$(h-agent-session-call "${agent}" name "${input}")"
    ret=$?
    if (( ret == 2 )) ; then
        h-agent-session-dep @RET
        name="$(agent_session "${agent}" name "${input}")" @RET
    elif (( ret != 0 )) ; then
        return "${ret}"
    fi
    name="${name//[^A-Za-z0-9._-]/-}"
    #: Collapse the runs a title's spaces and punctuation leave behind.
    name="${${name//---##/-}%%-##}"
    name="${name##-##}"

    if test -z "${name}" ; then
        h-agent-session-call "${agent}" id-of "${input}"
    else
        ec "${name}"
    fi
}

function h-agent-session-title {
    #: Emits the document header for a session, in the given syntax.
    #:
    #: The first line is a coding cookie. Emacs decides a file's encoding by
    #: sniffing it, and one stray byte in a transcript -- a command that
    #: printed a binary file, say -- makes it read the whole file as binary,
    #: at which point every non-ASCII character shows as an octal escape
    #: (`\302\267' for the `·' separators). The renderer strips those bytes
    #: (`ScrubText' in =internal/turns/helpers.go=), so this is the second
    #: line of defence rather than the fix; it costs one comment line.
    #: Usage: h-agent-session-title <org|md> <input>
    ##
    local syntax="${1}" input="${2}"

    local agent
    agent="$(h-agent-session-agent-of "${input}")" @RET

    local label id name account
    label="$(h-agent-field "${agent}" label)" @RET
    id="$(h-agent-session-call "${agent}" id-of "${input}")" @RET
    name="$(h-agent-session-name "${input}")" @RET
    #: Which seat this session came from: the Claude Code profile, or the
    #: signed-in account for an agent that has one. Two profiles are two
    #: accounts, and a rendered document said nothing about which was which.
    account="$(h-agent-session-call "${agent}" account "${input}" 2>/dev/null)" || account=''

    #: Which session this is and who ran it are two different facts, so they
    #: get two lines rather than one run-on string: `Codex session <uuid> ·
    #: someone@example.com · plus' reads as a single opaque identifier, and the
    #: account is the half a reader actually recognises. In org that second
    #: fact is `AUTHOR', which is org's own word for who produced a document
    #: and is exactly what an account is, so exporters give it its own line
    #: instead of rendering it as a tail on the subtitle.
    local subtitle="${label} session ${id}"

    if [[ "${syntax}" == org ]] ; then
        #: An org comment, so it is invisible in an exported document.
        ec "# -*- coding: utf-8 -*-"
        if [[ "${name}" == "${id}" ]] ; then
            #: The title already carries the id, so a subtitle repeating it
            #: would say nothing.
            ec "#+TITLE: ${label} Session ${id}"
        else
            ec "#+TITLE: ${name}"
            ec "#+SUBTITLE: ${subtitle}"
        fi
        test -n "${account}" && ec "#+AUTHOR: ${account}"
    else
        #: An HTML comment: `# ...' would be a markdown heading, and emacs
        #: reads the cookie out of the first line whatever its comment syntax.
        ec "<!-- -*- coding: utf-8 -*- -->"
        if [[ "${name}" == "${id}" ]] ; then
            ec "# ${label} Session ${id}"
        else
            ec "# ${name}"
            ec
            ec "${subtitle}"
        fi
        #: Its own paragraph, for the same reason org gets its own keyword.
        if test -n "${account}" ; then
            ec
            ec "${account}"
        fi
    fi
    ec
}

function h-agent-session-render {
    #: Renders a session transcript to stdout.
    #: Usage: h-agent-session-render <format> <input>
    ##
    local format="${1}" input="${2}"

    local agent
    agent="$(h-agent-session-agent-of "${input}")" @RET

    h-agent-session-dep @RET
    if [[ "${format}" == org-pandoc ]] ; then
        ensure-cmd pandoc @RET
    fi

    local render_args=("-format=${format}")
    local max_lines="${agent_session_max_block_lines:-${claude_code_session_max_block_lines:-0}}"
    render_args+=("-max-block-lines=${max_lines}")
    #: Unset leaves the renderer's own default, the CPU count. Set by
    #: [agfi:h-agent-view-convert], which has more than one conversion to
    #: think about; see [agfi:h-agent-view-jobs-share].
    local jobs="${agent_session_render_jobs:-${claude_code_session_render_jobs}}"
    if test -n "${jobs}" ; then
        render_args+=("-jobs=${jobs}")
    fi
    if bool "${agent_session_diff_p:-${claude_code_session_diff_p:-y}}" ; then
        render_args+=(-diff)
    else
        render_args+=(-diff=false)
    fi
    if bool "${agent_session_subagents_p:-${claude_code_session_subagents_p:-y}}" ; then
        render_args+=(-subagents)
    else
        render_args+=(-subagents=false)
    fi

    assert agent_session "${agent}" render "${render_args[@]}" "${input}" @RET
}

function h-agent-session-to-md {
    #: Converts a session transcript into a markdown file.
    ##
    local input="${1}"
    local out="${2:-${input:r}.md}"

    if ! test -e "${input}" ; then
        ecerr "$0: input file does not exist: ${input}"
        return 1
    fi

    {
        h-agent-session-title md "${input}" @RET
        h-agent-session-render md "${input}" @RET
    } > "${out}"
}

function h-agent-session-to-org-native {
    #: Converts a session transcript into an org-mode file, without pandoc.
    #: Message bodies stay markdown, so this is only a fallback; prefer
    #: [agfi:h-agent-session-to-org-pandoc].
    ##
    local input="${1}"
    local out="${2:-${input:r}.org}"

    if ! test -e "${input}" ; then
        ecerr "$0: input file does not exist: ${input}"
        return 1
    fi

    {
        h-agent-session-title org "${input}" @RET
        h-agent-session-render org "${input}" @RET
    } > "${out}"
}

function h-agent-session-to-org-pandoc {
    #: Converts a session transcript into an org-mode file. Emits intermediate
    #: markdown and lets pandoc do the org conversion, so the markdown message
    #: bodies become proper org markup.
    ##
    local input="${1}"
    local out="${2:-${input:r}.org}"

    if ! test -e "${input}" ; then
        ecerr "$0: input file does not exist: ${input}"
        return 1
    fi

    #: The pandoc run happens inside the renderer, split across processes;
    #: see "Performance" in =golang/agent_session/readme.org=.
    {
        h-agent-session-title org "${input}" @RET
        h-agent-session-render org-pandoc "${input}" @RET
    } > "${out}"
}
aliasfn h-agent-session-to-org h-agent-session-to-org-pandoc
##
#: Picking a transcript
##
function h-agent-session-fz {
    #: The one fzf every session picker goes through. Rows on stdin, tab
    #: separated: a caller column, the transcript, the agent, and then whatever
    #: the person choosing should see. Prints the chosen rows.
    #:
    #: The preview execs the Go binary itself with `{3}' for the agent -- fzf
    #: fills it in per row, so one picker can mix agents -- and alt+enter
    #: converts `{2}' in the background; see [agfi:h-agent-session-preview-cmd]
    #: and [agfi:h-agent-session-open-cmd]. alt+enter overrides the
    #: `print-query' that FZF_DEFAULT_OPTS binds to it, and nothing is lost: a
    #: printed query would come back as a row that fails the callers' `test -e'.
    #:
    #: `--ansi' because the preview is coloured. Anything after the mode is
    #: appended to fzf last, so an explicit `--preview-window hidden' there wins.
    #: Usage: ... | h-agent-session-fz <multi|no-multi> [fzf opts...]
    ##
    local multi="${1}"
    shift
    assert-args multi @RET

    local preview_cmd open_cmd
    preview_cmd="$(h-agent-session-preview-cmd '{3}')" @RET
    open_cmd="$(h-agent-session-open-cmd)" @RET

    #: An array, so an empty header is no `--header' at all rather than a blank
    #: row: a caller that does not want the advertisement sets the knob to the
    #: empty string. The old knob still wins when set, even to empty.
    local header="${agent_session_fz_header}"
    if (( ${+claude_code_session_fz_header} )) ; then
        header="${claude_code_session_fz_header}"
    fi
    local -a header_opt
    test -n "${header}" && header_opt=( --header "${header}" )

    fz_no_preview=y fz \
        --delimiter=$'\t' --with-nth='4..' "--${multi}" --ansi \
        --preview "${preview_cmd} {2}" \
        --preview-window 'down,60%,wrap' \
        --bind "alt-enter:execute-silent(${open_cmd} {2})" \
        "${header_opt[@]}" \
        "$@"
}

function h-agent-session-select-fz {
    #: Interactively selects a session transcript from the corpus and prints its
    #: path. Every agent in [agfi:h-agents], sorted together by last activity.
    ##
    local scope="${agent_session_fz_scope:-${claude_code_view_session_fz_scope:-project}}"
    ensure-array agent_session_fz_opts claude_code_view_session_fz_fz_opts
    local fz_opts=("${agent_session_fz_opts[@]}" "${claude_code_view_session_fz_fz_opts[@]}")

    h-agent-session-dep @RET

    #: `epoch<TAB>path<TAB>local time<TAB>name<TAB>relative path<TAB>snippet`,
    #: newest first, per agent; the agent is spliced in as the third column.
    #: The time is the last message's, not the file's mtime, and the name is
    #: empty for a session that has none; see =golang/agent_session/readme.org=.
    local -a list_args
    if bool "${agent_session_fz_subagents_p:-${claude_code_view_session_fz_subagents_p:-n}}" ; then
        #: Off by default: subagent transcripts are inlined into their parent
        #: by the renderer, so listing them here too is noise.
        list_args+=(-subagents)
    fi
    #: `project' scope is the adapter's business: `-cwd' lets each agent match
    #: this directory its own way (Claude encodes it into a directory name).
    if [[ "${scope}" != all ]] ; then
        list_args+=(-cwd "${PWD}")
    fi

    local -a agents
    agents=( ${(f)"$(h-agents)"} ) @TRET

    local agent glyph out lines=''
    local -a roots
    for agent in "${agents[@]}" ; do
        roots=( ${(f)"$(h-agent-session-call "${agent}" roots 2>/dev/null)"} ) || continue
        (( ${#roots} )) || continue

        #: An agent with no sessions here is normal, not an error.
        out="$(agent_session "${agent}" list "${list_args[@]}" "${roots[@]}" 2>/dev/null)" || continue
        test -n "${out}" || continue

        #: With several agents in one list, the glyph says which is which.
        glyph=''
        if (( ${#agents} > 1 )) ; then
            glyph="$(h-agent-field "${agent}" glyph) "
        fi
        lines+="$(ec "${out}" | gawk -F'\t' -v OFS='\t' -v a="${agent}" -v g="${glyph}" \
            '{ print $1, $2, a, $3, $4, g $5, $6 }')"$'\n'
    done
    lines="${lines%$'\n'}"

    if test -z "${lines}" ; then
        ecerr "$0: no sessions for scope '${scope}' (agents: ${(j:, :)agents})"
        return 1
    fi

    #: Newest first across agents; within one, this is the order `list` gave.
    local selected
    selected="$(ec "${lines}" | command sort -t $'\t' -k1,1nr -k2,2 |
        h-agent-session-fz no-multi "${fz_opts[@]}")" @RET
    selected="${selected%%$'\n'*}"

    local session_file="${${selected#*$'\t'}%%$'\t'*}"
    if ! test -e "${session_file}" ; then
        ecerr "$0: selected session file does not exist: ${session_file}"
        return 1
    fi

    ec "${session_file}"
}

function h-agent-view-session {
    #: Converts the given transcript using the given converter function, and
    #: opens the result in emacs.
    #: Usage: h-agent-view-session <converter> <ext> <session-file>
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
    name="$(h-agent-session-name "${session_file}")" @RET

    local out_file="${tmp_dir}/${name}.${ext}"
    if test -e "${out_file}" ; then
        out_file="${tmp_dir}/${name}-${${session_file:t:r}[1,8]}.${ext}"
    fi
    "${converter}" "${session_file}" "${out_file}" @RET

    emc-open "${out_file}" @RET
}

function h-agent-view-session-fz {
    #: Interactively selects a session, converts it using the given converter
    #: function, and opens the result in emacs.
    ##
    local converter="${1}"
    local ext="${2}"

    local session_file
    session_file="$(h-agent-session-select-fz)" @RET

    h-agent-view-session "${converter}" "${ext}" "${session_file}"
}

function agent-view-session {
    #: Converts the given session transcript to org-mode and opens it in emacs.
    #: The non-interactive counterpart of [agfi:agent-view-session-fz].
    ##
    h-agent-view-session h-agent-session-to-org org "${1}"
}

function agent-view-session-fz {
    #: Interactively selects a session, converts it to org-mode, and opens it
    #: in emacs.
    ##
    h-agent-view-session-fz h-agent-session-to-org org @RET
}
#: Same, but selects from the sessions of all projects.
aliasfn agent-view-session-all-fz agent_session_fz_scope=all agent-view-session-fz

function agent-view-session-md-fz {
    #: Interactively selects a session, converts it to markdown, and opens it
    #: in emacs.
    ##
    h-agent-view-session-fz h-agent-session-to-md md @RET
}
#: Same, but selects from the sessions of all projects.
aliasfn agent-view-session-md-all-fz agent_session_fz_scope=all agent-view-session-md-fz

function agent-view-session-raw-fz {
    #: Interactively selects a session and opens the original transcript in
    #: emacs.
    ##
    local session_file
    session_file="$(h-agent-session-select-fz)" @RET

    emc-open "${session_file}" @RET
}
#: Same, but selects from the sessions of all projects.
aliasfn agent-view-session-raw-all-fz agent_session_fz_scope=all agent-view-session-raw-fz
##
#: Reading the session you are *sitting in* should not need a picker: several
#: sessions often share a project directory, so "the newest one for this cwd"
#: is not reliably the right one. None of the agents can bind a key to a shell
#: command, so the keypress lives in kitty, and the hooks below leave it a note
#: saying which session runs in which window.
##
function h-agent-session-registry-dir {
    #: Where [agfi:agent-session-register] records which kitty window a session
    #: was last showing in.
    #:
    #: Not under `~/tmp'. That directory gets swept, which is how kitty's
    #: remote-control socket died (see docs/unix-sockets.md), and this registry
    #: vanished in the same sweep.
    ##
    ec "${agent_session_registry_dir:-${claude_code_session_registry_dir:-${XDG_STATE_HOME:-${HOME}/.local/state}/agent-sessions}}"
}

function h-agent-session-registry-key {
    #: kitty numbers its windows from 1 again every time it restarts, so its
    #: pid is what keeps a dead kitty's entries from being read as live ones.
    #: Usage: h-agent-session-registry-key <kitty-pid> <kitty-window-id>
    ##
    local kpid="${1}" win="${2}"

    if test -z "${kpid}" || test -z "${win}" ; then
        return 1
    fi

    ec "${kpid}-${win}"
}

function h-agent-session-registry-read {
    #: The transcript recorded for a registry key, or failure. The directory
    #: was `claude-code-sessions' before it served every agent; an entry
    #: written there is still honoured until the next kitty restart makes it
    #: stale anyway.
    #: Usage: h-agent-session-registry-read <key>
    ##
    local key="${1}"
    assert-args key @RET

    local dir entry t
    for dir in "$(h-agent-session-registry-dir)" "${XDG_STATE_HOME:-${HOME}/.local/state}/claude-code-sessions" ; do
        entry="${dir}/${key}"
        test -e "${entry}" || continue
        t="$(<"${entry}")"
        if test -n "${t}" && test -e "${t}" ; then
            ec "${t}"
            return 0
        fi
    done

    return 1
}

function agent-session-register {
    #: Records which kitty window the calling agent session is showing in, so
    #: [agfi:agent-view-session-focused] has something to fall back on when
    #: nothing else can tell. For the agents' prompt-submit and session-start
    #: hooks; the payload is JSON, taken from `$2' or stdin, and the adapter's
    #: `hook-transcript' verb reads the transcript out of it.
    #:
    #: This is the insurance layer. The live lookups in
    #: [agfi:h-agent-session-of-kitty-window] cover every way a session reaches
    #: a kitty window today, with no bookkeeping at all. This is for the way that
    #: does not exist yet, and for an agent whose process kitty cannot see.
    #:
    #: It records the one fact a hook has that nothing else does. When a prompt
    #: hook fires, Enter was just pressed, so the kitty window focused right now
    #: is the one showing this session -- whatever the attach mechanism.
    #:
    #: Two guards keep the record honest. No focused kitty window means the
    #: prompt did not come from one -- a message from another agent, the usage
    #: auto-continue -- and nothing is written. A focused window that resolves
    #: to a *different* session means the prompt was injected while you sat
    #: elsewhere, and nothing is written either. Only a window that cannot be
    #: resolved, or one that agrees, is recorded.
    #: Usage: agent-session-register <agent> [payload]
    ##
    local agent="${1}"
    assert-args agent @RET

    local transcript
    transcript="$(h-agent-session-call "${agent}" hook-transcript "${2}")" || return 0
    test -n "$transcript" || return 0

    #: Every early exit below means "not prompted from a kitty window", which is
    #: an ordinary outcome, not an error.
    local sock ls_json win kpid
    sock="$(h-agent-session-kitty-socket 2>/dev/null)" || return 0
    ls_json="$(kitty @ --to "${sock}" ls 2>/dev/null)" || return 0
    win="$(h-agent-session-focused-window "${ls_json}")" || return 0
    kpid="$(kitty-socket-pid "${sock}")" || return 0

    #: No kitty pid passed, so the resolver does not consult this registry
    #: while we are deciding what to put in it.
    local shown
    if shown="$(h-agent-session-of-kitty-window "${ls_json}" "${win}")" ; then
        [[ "${shown}" == "${transcript}" ]] || return 0
    elif ! h-agent-session-window-agent-p "${ls_json}" "${win}" ; then
        #: A window running no agent at all is not showing this session, so it
        #: must not be recorded as doing so. This matters for an agent whose
        #: hook fires more than once a turn -- Antigravity's PreInvocation runs
        #: per model call -- since by then the focus may have moved to an
        #: ordinary shell, which the two guards above would both accept.
        return 0
    fi

    local dir
    dir="$(h-agent-session-registry-dir)" @RET
    mkdir -p "$dir" @TRET

    local key
    key="$(h-agent-session-registry-key "${kpid}" "${win}")" @RET
    ec "$transcript" > "${dir}/${key}"
}

function h-agent-session-live-list {
    #: Every live session of every agent in [agfi:h-agents], one per line, tab
    #: separated: pid, session id, name, cwd, transcript, tmux session (or `-'),
    #: status. Each adapter's `live-list' verb supplies its rows; an agent
    #: without one contributes nothing.
    #:
    #: Set agent_session_live_list_cache to reuse one listing across several
    #: lookups; `local' is dynamically scoped, so a caller's assignment is
    #: visible here.
    ##
    local cache="${agent_session_live_list_cache:-${claude_code_session_live_list_cache}}"
    if test -n "${cache}" ; then
        ec "${cache}"
        return 0
    fi

    #: One call for every agent, which the binary answers concurrently and with
    #: a single process table and `tmux list-panes' between them. Asking each
    #: adapter in turn cost about a second before anything reached the screen:
    #: `claude agents --json' 190ms, the process table 115ms twice over, two
    #: `lsof' runs, three binary starts. Batched it is around 200ms.
    local -a specs
    local agent root
    for agent in ${(f)"$(h-agents)"} ; do
        for root in ${(f)"$(h-agent-session-call "${agent}" roots 2>/dev/null)"} ; do
            test -n "${root}" || continue
            specs+=( "${agent}=${root}" )
        done
    done

    if (( ${#specs} )) && h-agent-session-dep 2>/dev/null ; then
        #: An agent the binary has no adapter for makes it refuse the whole
        #: call, and so does a missing binary; either way the loop below still
        #: knows how to ask each adapter itself.
        if agent_session live-all "${specs[@]}" 2>/dev/null ; then
            return 0
        fi
    fi

    local out rows=''
    for agent in ${(f)"$(h-agents)"} ; do
        out="$(h-agent-session-call "${agent}" live-list)" || continue
        test -n "${out}" || continue
        rows+="${out}"$'\n'
    done

    ec "${rows%$'\n'}"
}

function h-agent-session-row-transcript {
    #: The transcript of a [agfi:h-agent-session-live-list] row, if the file
    #: exists yet. A session that has written nothing is not a target: there is
    #: nothing to show.
    ##
    local -a f
    f=( "${(@ps:\t:)1}" )

    local t="${f[5]}"
    test -n "${t}" && test -e "${t}" || return 1
    ec "${t}"
}

function h-agent-session-name-of-transcript {
    #: The live session's name for a transcript, or `-'.
    ##
    local t="${1}"

    local -a live f
    live=( ${(f)"$(h-agent-session-live-list)"} )

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

function h-agent-session-tmux-target {
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

function h-agent-session-focused-window {
    #: The id of kitty's focused window, from `kitty @ ls' output. Fails when
    #: no OS window is focused, which is what kitty reports whenever it is not
    #: the frontmost application.
    ##
    local win
    win="$(ec "${1}" | jq -r 'first(.[] | select(.is_focused) | .tabs[] | select(.is_focused) | .windows[] | select(.is_focused) | .id) // empty' 2>/dev/null)"
    test -n "${win}" || return 1
    ec "${win}"
}

function h-agent-session-cmds-agent-p {
    #: Whether any of the given command lines runs an agent: its own binary, or
    #: a shell that was handed one. The binary names come from
    #: [agfi:h-agents-table], so an agent added there is recognised here too.
    #: Usage: h-agent-session-cmds-agent-p <cmdline>...
    ##
    local -a bins
    bins=( ${=$(h-agents-table | command cut -f4)} )

    local cmd
    local -a w
    for cmd in "$@" ; do
        w=( ${(z)cmd} )
        if (( ${bins[(Ie)${w[1]:t}]} )) ; then
            return 0
        fi
        if [[ "${w[1]:t}" == (sh|bash|dash|zsh) ]] && (( ${bins[(Ie)${w[2]:t}]} )) ; then
            return 0
        fi
    done
    return 1
}

function h-agent-session-window-agent-p {
    #: Whether a kitty window is plausibly showing an agent at all: it runs one
    #: itself, or a tmux client (behind which one may sit).
    #: Usage: h-agent-session-window-agent-p <kitty ls json> <window id>
    ##
    local ls_json="${1}" win="${2}"
    test -n "${ls_json}" && test -n "${win}" || return 1

    local -a row cmds
    row=( "${(@ps:\t:)$(h-agent-session-window-row "${ls_json}" "${win}")}" )
    (( ${#row} )) || return 1
    cmds=( "${(@ps:\037:)row[4]}" )

    h-agent-session-cmds-agent-p "${cmds[@]}" && return 0

    local cmd
    for cmd in "${cmds[@]}" ; do
        h-agent-session-tmux-target "${cmd}" >/dev/null && return 0
    done
    return 1
}

function h-agent-session-tmux-identities {
    #: `<session name><TAB><@agent_session>' for every tmux session that has
    #: the option set, from one `tmux list-sessions'.
    #:
    #: The hooks write the option per session ([agfi:h-agent-tmux-autoname]),
    #: and asking `show-option' once per kitty window meant a tmux round trip
    #: per window. One listing answers for all of them.
    ##
    command tmux list-sessions -F \
        "#{session_name}"$'\t'"#{${agent_tmux_identity_option:-@agent_session}}" 2>/dev/null
}

function h-agent-session-tmux-clients {
    #: `<client pid><TAB><session name>' for every attached tmux client, from
    #: one `tmux list-clients'.
    #:
    #: Which session a kitty window's tmux client is *actually* attached to,
    #: rather than the name its command line happens to spell. The autoname
    #: hooks rename a session after the agent inside it, so a window running
    #: `tmux attach -t scripts-claudework1' is attached to a session now called
    #: `+Claude/work <name>' -- and matching the command line's name against
    #: anything current cannot work.
    ##
    command tmux list-clients -F "#{client_pid}"$'\t'"#{client_session}" 2>/dev/null
}

function h-agent-session-tmux-client-session {
    #: The tmux session a client pid is attached to, from the cache when a
    #: caller primed `agent_session_tmux_clients' and a fresh listing
    #: otherwise.
    #: Usage: h-agent-session-tmux-client-session <pid>
    ##
    local pid="${1}"
    test -n "${pid}" || return 1

    local table="${agent_session_tmux_clients}"
    if test -z "${table}" ; then
        table="$(h-agent-session-tmux-clients)" || return 1
    fi

    local row
    for row in ${(f)table} ; do
        if [[ "${row%%$'\t'*}" == "${pid}" ]] ; then
            row="${row#*$'\t'}"
            test -n "${row}" || return 1
            print -r -- "${row}"
            return 0
        fi
    done
    return 1
}

function h-agent-session-tmux-identity {
    #: One tmux session's `@agent_session' value, from the cache when a caller
    #: primed `agent_session_tmux_identities' and from a fresh listing
    #: otherwise. Fails when the session has none.
    #: Usage: h-agent-session-tmux-identity <tmux session name>
    ##
    local tname="${1}"
    test -n "${tname}" || return 1

    local table="${agent_session_tmux_identities}"
    if test -z "${table}" ; then
        table="$(h-agent-session-tmux-identities)" || return 1
    fi

    local row
    for row in ${(f)table} ; do
        if [[ "${row%%$'\t'*}" == "${tname}" ]] ; then
            row="${row#*$'\t'}"
            test -n "${row}" || return 1
            print -r -- "${row}"
            return 0
        fi
    done
    return 1
}

function h-agent-session-windows {
    #: One line per kitty window: id, title, its foreground pids (space
    #: separated), and their command lines (separated by a unit separator,
    #: which no command line carries).
    #:
    #: One `jq' for the whole listing. The pickers resolve every window in
    #: turn, and a `jq' per window per field was most of what they spent:
    #: twenty windows meant forty processes for one listing already in hand.
    #: Set `agent_session_windows_cache' to this and every resolution reuses
    #: it, the same way `agent_session_live_list_cache' works.
    #: Usage: h-agent-session-windows <kitty ls json>
    ##
    local ls_json="${1}"
    test -n "${ls_json}" || return 1

    ec "${ls_json}" | jq -r '
        .[].tabs[].windows[] | [
            (.id | tostring),
            (.title // ""),
            ([.foreground_processes[]? | .pid | tostring] | join(" ")),
            ([.foreground_processes[]? | (.cmdline // []) | join(" ")] | join("\u001f"))
        ] | @tsv'
}

function h-agent-session-window-row {
    #: One window's row of [agfi:h-agent-session-windows], from the cache when
    #: a caller set one and from a fresh read otherwise.
    #: Usage: h-agent-session-window-row <kitty ls json> <window id>
    ##
    local ls_json="${1}" win="${2}"

    local table="${agent_session_windows_cache}"
    if test -z "${table}" ; then
        table="$(h-agent-session-windows "${ls_json}")" || return 1
    fi

    local row
    for row in ${(f)table} ; do
        if [[ "${row%%$'\t'*}" == "${win}" ]] ; then
            print -r -- "${row}"
            return 0
        fi
    done
    return 1
}

function h-agent-session-of-kitty-window {
    #: The transcript of the agent session showing in a kitty window, or
    #: failure when that cannot be established.
    #:
    #: A session reaches a kitty window several ways, each leaving a different
    #: trace, so this tries them in order of how sure each one is:
    #:
    #: 0. The window shows a tmux client, and the hooks have recorded who
    #:    lives in the session that client is attached to (=agent-tmux.zsh=).
    #:    The session is found from the client's *pid* through `tmux
    #:    list-clients', not from the name its command line spells: the hooks
    #:    rename a session after the agent in it, so `tmux attach -t
    #:    scripts-claudework1' is attached to something now called
    #:    `+Claude/work <name>'. Tried first because it is the one answer that
    #:    needs no live listing, and a listing costs 200ms -- `claude agents
    #:    --json' alone is 190ms of it. It cannot contradict the tests below
    #:    either: a window showing a tmux client has none of the agent's own
    #:    processes in the foreground, so the pid test would not have fired.
    #: 1. The agent runs in the window itself: a foreground pid of the window
    #:    is a live session's pid.
    #: 2. The window shows a tmux client whose NAME matches the live rows' tmux
    #:    column, for a session whose hooks have not written the option.
    #: 3. The window shows an agent's own view (`claude agents', `claude
    #:    attach'): the view sets the window title to the attached session's
    #:    name, at times with a status glyph in front. Observed rather than
    #:    documented, so it has to match exactly one live session or it is
    #:    ignored, and only when a foreground process is one of the agents'
    #:    binaries.
    #: 4. The registry [agfi:agent-session-register] keeps as insurance: where
    #:    the session was showing the last time it was prompted.
    #:
    #: The first three read only what exists right now and so cannot go stale;
    #: the fourth can, which is why it comes last. Pass no kitty pid to skip it,
    #: as the hook does when deciding whether to *write* it.
    #: Usage: h-agent-session-of-kitty-window <kitty ls json> <window id> [kitty pid]
    ##
    local ls_json="${1}" win="${2}" kpid="${3}"
    test -n "${ls_json}" && test -n "${win}" || return 1

    local -a row
    row=( "${(@ps:\t:)$(h-agent-session-window-row "${ls_json}" "${win}")}" )
    (( ${#row} )) || return 1

    local title="${row[2]}"
    local -a fg_pids fg_cmds
    fg_pids=( ${=row[3]} )
    fg_cmds=( "${(@ps:\037:)row[4]}" )

    local -a f
    local cmd tname identity

    #: 0. The hooks' record on the tmux session this window's client is
    #: attached to: `agent<TAB>id<TAB>transcript'. Before the live listing,
    #: since this needs none.
    local pid
    for pid in "${fg_pids[@]}" ; do
        tname="$(h-agent-session-tmux-client-session "${pid}")" || continue
        identity="$(h-agent-session-tmux-identity "${tname}")" || continue

        f=( "${(@ps:\t:)identity}" )
        if test -n "${f[3]}" && test -e "${f[3]}" ; then
            ec "${f[3]}"
            return 0
        fi
    done

    local agent_session_live_list_cache="${agent_session_live_list_cache:-$(h-agent-session-live-list)}"
    local -a live hits
    live=( ${(f)agent_session_live_list_cache} )

    local row
    #: 1. The agent in the window itself.
    for row in "${live[@]}" ; do
        if (( ${fg_pids[(Ie)${row%%$'\t'*}]} )) ; then
            h-agent-session-row-transcript "${row}" && return 0
        fi
    done

    #: 2. A tmux client whose session the hooks never recorded.
    for cmd in "${fg_cmds[@]}" ; do
        tname="$(h-agent-session-tmux-target "${cmd}")" || continue

        hits=()
        for row in "${live[@]}" ; do
            f=( "${(@ps:\t:)row}" )
            [[ "${f[6]}" == "${tname}" ]] && hits+=("${row}")
        done
        #: One tmux session can host several agents in several panes; then
        #: the window alone does not say which is meant.
        if (( ${#hits} == 1 )) ; then
            h-agent-session-row-transcript "${hits[1]}" && return 0
        fi
    done

    #: 3. An agent's view, by title.
    local attach_p=n
    if h-agent-session-cmds-agent-p "${fg_cmds[@]}" ; then
        attach_p=y
    fi
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
            h-agent-session-row-transcript "${hits[1]}" && return 0
        fi
    fi

    #: 4. The registry.
    if test -n "${kpid}" ; then
        local key
        if key="$(h-agent-session-registry-key "${kpid}" "${win}")" ; then
            h-agent-session-registry-read "${key}" && return 0
        fi
    fi

    return 1
}

function h-agent-session-pick-overlay {
    #: Opens the session picker as a kitty overlay over the active window, for
    #: when the window itself does not say which session it shows.
    #:
    #: It runs as a small script, =zshlang/wrappers/agent-session-pick.zsh=,
    #: because fzf needs a terminal and nothing on this side has one: the hotkey
    #: runs in the background and the garden's shells are not interactive. The
    #: script gets its rows and hands its choice back through the garden.
    #:
    #: kitty's own environment has the bare macOS PATH, so ours is passed in.
    #: The tab key, when given, travels the same way, so the picker's choice
    #: comes back through [agfi:agent-view-session-bg] under the tab it was
    #: opened from, with the same band and cancel as the hotkey.
    #: Usage: h-agent-session-pick-overlay <kitty socket> [tab-key]
    ##
    local sock="${1}" key="${2}"
    assert-args sock @RET

    local wrappers="${NIGHTDIR:-${HOME}/scripts}/zshlang/wrappers"

    kitty @ --to "${sock}" launch --type=overlay --title 'Agent sessions' \
        --env "PATH=${PATH}" \
        --env "NIGHTDIR=${NIGHTDIR:-${HOME}/scripts}" \
        --env "FZF_DEFAULT_OPTS=${FZF_DEFAULT_OPTS}" \
        --env "AGENT_VIEW_TAB_KEY=${key}" \
        "${wrappers}/zshplain.dash" "${wrappers}/agent-session-pick.zsh" >/dev/null
}

function h-agent-session-kitty-socket {
    #: The kitty instance to talk to, as `unix:<path>'.
    #:
    #: A thin wrapper over [agfi:kitty-socket-get], which owns everything about
    #: where the socket lives and which kitty owns it. It used to demand
    #: *exactly one* match from the glob, so a single socket left behind by a
    #: crashed kitty was enough to break this; the shared resolver filters by
    #: live pid instead, and so needs no such rule.
    #:
    #: On failure the reason is on stderr, which
    #: [agfi:h-agent-view-session-focused] captures verbatim -- the reason is
    #: the whole point, since "no socket" and "kitty is running but its socket
    #: is gone, restart it" want completely different responses.
    #:
    #: `agent_session_kitty_socket_glob' stays as the documented override for
    #: the tests; zsh scopes it dynamically, so assigning it here is visible to
    #: the resolver.
    ##
    local kitty_sockets_list_glob="${agent_session_kitty_socket_glob:-${claude_code_session_kitty_socket_glob:-${kitty_sockets_list_glob}}}"

    kitty-socket-get
}

function h-agent-session-lost {
    #: The hotkey runs detached, so stderr goes nowhere a person will look.
    ##
    ecerr "agent-view-session-focused: ${1}"
    silence notif "Agent session: ${1}"
    return 1
}

function h-agent-session-label-of {
    #: `Claude Code session', `Codex session', ... for a transcript, for the
    #: bands and notifications; `Agent session' when the agent is not known.
    ##
    local transcript="${1}"

    local agent label=''
    if test -n "${transcript}" && agent="$(h-agent-session-agent-of "${transcript}" 2>/dev/null)" ; then
        label="$(h-agent-field "${agent}" label 2>/dev/null)" || label=''
    fi
    ec "${label:-Agent} session"
}

function agent-view-session-focused {
    #: Opens the agent session running in the focused kitty window as an org
    #: file in emacs. Bound to a kitty hotkey; the window -> session mapping
    #: comes from [agfi:h-agent-session-of-kitty-window].
    #:
    #: Always succeeds. Every way this comes up empty -- no session in this
    #: window, kitty not answering -- is a message for a person, and it has
    #: already been delivered as a notification by the time we get here. A
    #: non-zero return would only add BrishGarden's failed-command bell on top
    #: of it, for something that is not a failure.
    ##
    h-agent-view-session-focused || true
}

function h-agent-view-session-focused {
    #: The body of [agfi:agent-view-session-focused], split out so the hotkey
    #: can swallow the exit code without swallowing the reason.
    #:
    #: A toggle. The tab is identified (about 30ms, kitty only); if a conversion
    #: is already running for it, this press cancels it. Otherwise the tab is
    #: claimed and the work goes to the background: the 300ms session lookup,
    #: the conversion and the emacs open all happen in [agfi:h-agent-view-job],
    #: behind a band that names the session and says a second press cancels.
    #: Resolving *before* claiming would leave a 300ms window in which a second
    #: press starts a second job; this way a double-tap deterministically
    #: cancels.
    #:
    #: Everything that is a failure reports through [agfi:h-agent-session-lost];
    #: "which one did you mean" is a question and gets the picker instead.
    ##
    if ! ensure-cmd kitty jq ; then
        h-agent-session-lost "kitty or jq is not installed"
        return 1
    fi

    #: stdout and stderr together: on success this is the socket, on failure it
    #: is the reason, and a subshell cannot hand a variable back to us.
    local sock
    sock="$(h-agent-session-kitty-socket 2>&1)"
    if [[ "${sock}" != unix:* ]] ; then
        h-agent-session-lost "${${sock#kitty-socket-get: }:-could not find kitty's socket}"
        return 1
    fi

    local ls_json
    if ! ls_json="$(kitty @ --to "${sock}" ls)" ; then
        h-agent-session-lost "kitty did not answer on ${sock}"
        return 1
    fi

    local win
    if ! win="$(h-agent-session-focused-window "${ls_json}")" ; then
        h-agent-session-lost "could not identify the focused kitty window"
        return 1
    fi

    local kpid key
    kpid="$(kitty-socket-pid "${sock}")" @RET
    key="$(h-agent-session-registry-key "${kpid}" "${win}")" @RET

    local name
    name="$(h-agent-view-name-of "${key}")" @RET

    if tmux-alive-p "${name}" ; then
        h-agent-view-cancel "${key}"
        return 0
    fi

    h-agent-view-launch "${key}" \
        "**Agent session** → org: finding the session…   (⌘⇧O again cancels)" \
        h-agent-view-job "${key}" "${win}" "${sock}"
}

function agent-view-session-bg {
    #: Converts a known transcript to org and opens it in emacs, in the
    #: background, under the same per-tab band as the hotkey. For the overlay
    #: picker, whose choice arrives with the key of the tab it was opened from.
    #:
    #: Starts, and only starts. It is deliberately not the toggle the hotkey is,
    #: even though it shares the key: its caller is the overlay's Enter, which
    #: runs while the job that opened the overlay may still be winding down --
    #: [agfi:h-agent-view-job] launches the picker from *inside* that tmux
    #: session and only then returns. A toggle here would sometimes answer "I
    #: picked this one" with "cancelled", silently. The picker's own alt+enter
    #: is where toggling belongs; see [agfi:agent-view-session-toggle].
    #: Usage: agent-view-session-bg <transcript> <tab-key>
    ##
    local transcript="${1}" key="${2}"
    assert-args transcript key @RET

    h-agent-view-launch "${key}" \
        "**$(h-agent-session-label-of "${transcript}")** → org: starting…   (⌘⇧O again cancels)" \
        h-agent-view-convert "$(h-agent-view-name-of "${key}")" '⌘⇧O' "${transcript}"
}

function agent-view-session-toggle {
    #: Converts a transcript to org in the background and opens it in emacs, or
    #: cancels that transcript's conversion when one is already running. What
    #: alt+enter in the session pickers is bound to.
    #:
    #: Keyed on the transcript rather than on a kitty window, unlike the hotkey.
    #: A picker moves between rows, so "press again" has to mean "again on this
    #: row"; keying on the window would make alt+enter on a *different* row
    #: cancel the running conversion instead of starting the new one. Two
    #: consequences, both wanted: several conversions can run at once and their
    #: bands stack, and nothing here needs kitty, so this works over ssh and on
    #: Linux (only the band is macOS-only, and it already fails soft).
    #:
    #: The flip side is that ⌘⇧O cannot cancel one of these and this cannot
    #: cancel one of ⌘⇧O's: they are separate key namespaces on purpose.
    #: Usage: agent-view-session-toggle <transcript>
    ##
    local transcript="${1}"

    #: Neither of these is a row worth a band, and both arrive by ordinary use:
    #: fzf fires the binding with an empty `{2}' when nothing matches, and the
    #: live picker's synthetic "frontmost" row has `-' in that column.
    if test -z "${transcript}" || [[ "${transcript}" == '-' ]] ; then
        return 0
    fi

    local key
    key="$(h-agent-view-transcript-key "${transcript}")" @RET

    #: Checked here rather than left to [agfi:h-agent-view-convert], which only
    #: reaches its own `test -e' after a tmux session, a renderer build check
    #: and a temp directory have been created for nothing.
    if ! test -e "${transcript}" ; then
        h-agent-view-fail "$(h-agent-view-name-of "${key}")" '' \
            "no transcript on disk: ${transcript:t}"
        return 1
    fi

    local name
    name="$(h-agent-view-name-of "${key}")" @RET

    if tmux-alive-p "${name}" ; then
        h-agent-view-cancel "${key}"
        return 0
    fi

    h-agent-view-launch "${key}" \
        "**$(h-agent-session-label-of "${transcript}")** → org: starting…   (⌥⏎ again cancels)" \
        h-agent-view-convert "${name}" '⌥⏎' "${transcript}"
}

function h-agent-view-transcript-key {
    #: The key a transcript's own conversion runs under: its id, which is what
    #: the file (or, for Antigravity, its directory) is named. Sanitised
    #: because the name becomes a tmux session: tmux forbids `:' and `.', and a
    #: Claude subagent transcript is `<session>/subagents/<name>.jsonl', whose
    #: basename is not a uuid at all.
    #: Usage: h-agent-view-transcript-key <transcript>
    ##
    local transcript="${1}"
    assert-args transcript @RET

    local agent key
    if agent="$(h-agent-session-agent-of "${transcript}" 2>/dev/null)" ; then
        key="$(h-agent-session-call "${agent}" id-of "${transcript}")" || key=''
    fi
    key="${key:-${transcript:t:r}}"
    key="${key//[^A-Za-z0-9-]/-}"
    #: Collapse the runs the substitution leaves behind, and refuse to hand back
    #: a name that is all separator.
    key="${${key//---##/-}##-##}"
    key="${key%%-##}"

    if test -z "${key}" ; then
        ecerr "$0: could not derive a key from: ${transcript}"
        return 1
    fi

    ec "${key}"
}

#: The prefix every conversion's tmux session and hs-alert band share. Its own
#: variable because [agfi:agent-view-sessions] scans for it and
#: [agfi:h-agent-view-name-of] builds from it.
typeset -g agent_view_session_prefix="${agent_view_session_prefix:-${claude_code_view_session_prefix:-agent-view-}}"

function agent-view-sessions {
    #: Every conversion job's tmux session, one per line, alive or a finished
    #: leftover.
    #:
    #: A prefix scan, the way [agfi:caffeinate-holders] does it, rather than
    #: regenerating a list of known names the way
    #: [agfi:claude-code-usage-notif-sessions] can: the hotkey's keys are
    #: bounded by the number of kitty windows, but a picker's are one per
    #: transcript, so there is no finite set of names to reconstruct.
    #:
    #: The old `claude-view-' prefix is scanned too, so leftovers from before
    #: the rename are reaped.
    ##
    local out
    out="$(tmux list-sessions -F '#{session_name}' 2>/dev/null)" || return 0

    local line
    for line in "${(@f)out}" ; do
        if [[ "${line}" == ("${agent_view_session_prefix}"|claude-view-)* ]] ; then
            ec "${line}"
        fi
    done
}

function agent-view-reap {
    #: Removes the tmux sessions of conversions that have already finished, and
    #: their temp directories with them.
    #:
    #: They accumulate because =remain-on-exit= is on, so a finished job leaves
    #: its session behind. Elsewhere that is the point --- it is how "did my
    #: notifier already fire?" is answered --- but a conversion has a band and a
    #: buffer to show for itself, and under a picker's per-transcript keys it is
    #: one dead session for every transcript ever opened rather than one per
    #: kitty window.
    #:
    #: [agfi:h-agent-view-launch] calls this, so the thing that creates them
    #: clears them and nothing has to be scheduled. Safe to run by hand.
    ##
    local s reaped=0 tmp
    for s in ${(@f)"$(agent-view-sessions)"} ; do
        test -n "${s}" || continue
        #: Alive means a pane that is not dead; `has-session' alone cannot tell,
        #: which is the whole reason [agfi:tmux-alive-p] exists.
        if tmux-alive-p "${s}" ; then
            continue
        fi

        #: Read before the kill: killing the session discards its options.
        tmp="$(tmux show-options -qv -t "${s}" '@ccv_tmp' 2>/dev/null)" || tmp=''
        h-agent-view-rm-tmp "${tmp}"
        silent tmux kill-session -t "=${s}" || true
        reaped=$(( reaped + 1 ))
    done

    if (( reaped )) ; then
        ecgray "$0: reaped ${reaped} finished conversion(s)"
    fi
    return 0
}

function h-agent-view-jobs-share {
    #: How many workers one conversion should ask the renderer for: this
    #: machine's share of it, divided by the number of conversions running.
    #:
    #: `-jobs' defaults to the CPU count and `pandocChunks' spawns that many
    #: pandocs, which was fine while the hotkey allowed one conversion per kitty
    #: window. A picker keys them per transcript and stays open to invite
    #: pressing again, so alt+enter down a list of ten used to mean ten times
    #: the CPU count in pandoc processes. Dividing keeps the total at roughly
    #: one machine's worth however many are started, and refuses nothing.
    #:
    #: Counted here rather than at launch, and that is what makes it safe
    #: without a lock: `tmuxnewsh2' creates the session before the body runs, so
    #: a conversion always counts itself. Two starting at once therefore both
    #: see two and both take half --- which is the right answer, not a race. A
    #: redis token would need releasing, and the main verb here is cancel, which
    #: SIGKILLs the job; liveness is a property of the process, so this
    #: self-heals where a record would drift.
    #:
    #: Conversions already running keep the larger share they were given. Their
    #: chunks are short-lived, so the overshoot drains rather than persisting.
    ##
    local cpus="${agent_session_render_jobs_max:-${claude_code_session_render_jobs_max}}"
    if test -z "${cpus}" ; then
        cpus="$(getconf _NPROCESSORS_ONLN 2>/dev/null)" || cpus=''
    fi
    #: A machine that will not say gets the renderer's own default.
    if [[ "${cpus}" != <-> ]] || (( cpus < 1 )) ; then
        return 1
    fi

    local s live=0
    for s in ${(@f)"$(agent-view-sessions)"} ; do
        test -n "${s}" || continue
        if tmux-alive-p "${s}" ; then
            live=$(( live + 1 ))
        fi
    done
    #: Nothing alive means we are not running under a job at all, so speak for
    #: one conversion rather than dividing by zero.
    (( live < 1 )) && live=1

    local share=$(( cpus / live ))
    (( share < 1 )) && share=1
    ec "${share}"
}

function h-agent-view-name-of {
    #: The tmux session, and the alert id, that a conversion runs under:
    #: `agent-view-<key>'. One name for both, so what `tmux ls' shows and what
    #: is on screen line up.
    #:
    #: Two kinds of key reach here, and they are separate namespaces by design:
    #: `<kitty-pid>-<window-id>' for the hotkey, from
    #: [agfi:h-agent-session-registry-key], and a transcript's id for a
    #: picker's alt+enter, from [agfi:h-agent-view-transcript-key]. tmux
    #: forbids `:' and `.' in session names; neither kind contains either.
    #: Usage: h-agent-view-name-of <key>
    ##
    local key="${1}"
    assert-args key @RET

    ec "${agent_view_session_prefix}${key}"
}

function h-agent-view-banner {
    #: The per-tab band, through hs-alert v2. Re-showing an id updates that band
    #: in place, and an unchanged message on an existing id is a heartbeat, so
    #: calling this again with new text is how the job reports progress. A
    #: flash of 0 for updates: a changed message on an existing id would
    #: otherwise flash the screen again.
    #:
    #: Wrapped in `reval-timeout' like every zsh caller of hs-alert, and never
    #: allowed to fail the caller: a Hammerspoon that is mid-reload must not
    #: hang a hotkey or fail a conversion.
    #: Usage: h-agent-view-banner <name> <color> <seconds> <flash-seconds> <md text>
    ##
    local name="${1}" color="${2}" dur="${3}" flash="${4}"
    shift 4
    local text="$*"

    silence reval-timeout 10 \
        @opts id "${name}" color "${color}" dur "${dur}" flash "${flash}" markup md @ \
        alert "${text}" || true
}

function h-agent-view-dismiss {
    #: Takes a tab's band down. Same guards as [agfi:h-agent-view-banner].
    #: Usage: h-agent-view-dismiss <name>
    ##
    silence reval-timeout 10 hs-alert-dismiss "${1}" || true
}

function h-agent-view-launch {
    #: Claims a tab and starts its conversion in the background.
    #:
    #: The job lives in a tmux session named for the tab, the repository's
    #: detached-job primitive: `tmuxnew' kills any previous session of that
    #: name before creating the new one, `tmux-alive-p' says whether the job is
    #: still running, and `tmux-session-processes-kill' takes the whole process
    #: tree down. The session name is the lock, the handle and the cancel
    #: target at once -- no marker, pid file or redis key -- exactly as the
    #: usage notifier arms itself ([agfi:h-claude-code-usage-notif]).
    #:
    #: Two things are done here, in the foreground, so a cancel never has to
    #: kill them: the renderer's build check (a first use compiles Go) and the
    #: temp directory, which is recorded on the tmux session for the canceller
    #: to remove -- bookkeeping that cannot outlive the job.
    #: Usage: h-agent-view-launch <tab-key> <banner text> <fn> [args...]
    #: The function receives its args followed by the temp directory.
    ##
    local key="${1}" text="${2}"
    shift 2
    local -a job
    job=("$@")
    (( ${#job} )) || return 1

    local name
    name="$(h-agent-view-name-of "${key}")" @RET

    #: Finished jobs first, so their sessions and temp directories do not
    #: outlive their usefulness; see [agfi:agent-view-reap].
    silent agent-view-reap || true

    #: `tmuxnew' inside `tmuxnewsh2' replaces any session of this name, and that
    #: discards its options -- so the predecessor's temp directory has to be
    #: read and removed here or it is orphaned for good. Two ways that happened:
    #: a press inside the window between our callers' `tmux-alive-p' test and the
    #: claim below, which starts a second launch and strands the first one's
    #: directory; and a finished job's leftover, whose pane is dead but whose
    #: session and options survive `remain-on-exit'.
    local prev_tmp
    prev_tmp="$(tmux show-options -qv -t "${name}" '@ccv_tmp' 2>/dev/null)" || prev_tmp=''
    h-agent-view-rm-tmp "${prev_tmp}"

    h-agent-session-dep @RET

    local tmp_dir
    tmp_dir="$(gmktemp --directory)" @TRET

    h-agent-view-banner "${name}" notice 600 0.35 "${text}"

    #: `silent': `tmuxnew' narrates the kill of any previous session.
    if ! silent tmuxnewsh2 "${name}" "${job[@]}" "${tmp_dir}" ; then
        h-agent-view-fail "${name}" "${tmp_dir}" "could not start the conversion job"
        return 1
    fi

    #: No `=' exact-match prefix on the target: unlike =has-session=,
    #: =set-option= does not accept one.
    silent tmux set-option -t "${name}" '@ccv_tmp' "${tmp_dir}" || true
}

function h-agent-view-job {
    #: The body of the hotkey's conversion, inside the tmux session
    #: [agfi:h-agent-view-launch] creates. A function, because a bare command
    #: would not keep the session alive.
    #:
    #: Resolves the window the key was pressed in -- by id, not "the focused
    #: one": focus may have moved during the lookup -- and converts what it
    #: finds. When it finds nothing, the picker takes over the screen, so the
    #: band goes and the temp directory with it; the picker's choice comes back
    #: through [agfi:agent-view-session-bg] under this same key.
    #: Usage: h-agent-view-job <tab-key> <window-id> <kitty socket> <tmp dir>
    ##
    local key="${1}" win="${2}" sock="${3}" tmp_dir="${4}"
    assert-args key win sock tmp_dir @RET

    local name
    name="$(h-agent-view-name-of "${key}")" @RET

    local ls_json
    if ! ls_json="$(kitty @ --to "${sock}" ls)" ; then
        h-agent-view-fail "${name}" "${tmp_dir}" "kitty did not answer on ${sock}"
        return 1
    fi

    local kpid
    kpid="$(kitty-socket-pid "${sock}")" || kpid=''

    local transcript
    if ! transcript="$(h-agent-session-of-kitty-window "${ls_json}" "${win}" "${kpid}")" ; then
        h-agent-view-dismiss "${name}"
        command rm -rf -- "${tmp_dir}"

        if ! h-agent-session-pick-overlay "${sock}" "${key}" ; then
            h-agent-session-lost "could not tell which session this window shows, and could not open the picker either"
            return 1
        fi
        return 0
    fi

    h-agent-view-convert "${name}" '⌘⇧O' "${transcript}" "${tmp_dir}"
}

function h-agent-view-convert {
    #: Converts one transcript to org under the given band and opens it in
    #: emacs. The band is updated with the session's name as soon as it is
    #: known, and taken down once emacs has the file. On failure it turns red
    #: with the reason, a notification is sent as well, and the temp directory
    #: is removed.
    #:
    #: On success the temp directory stays, as it always has: emacs has the file
    #: open.
    #:
    #: The cancel hint is an argument because this repaint is shared by every
    #: entry point and they are not cancelled by the same key: it used to say
    #: `⌘⇧O' unconditionally, so a conversion started from a picker spent its
    #: whole life advertising a key that would not cancel it. It cannot be a
    #: `local' in the caller either --- this runs inside the tmux session, a
    #: different process, so dynamic scope does not reach here. The same goes
    #: for the agent: it is read off the transcript's path, never passed.
    #: Usage: h-agent-view-convert <name> <cancel hint> <transcript> <tmp dir>
    ##
    local name="${1}" hint="${2}" transcript="${3}" tmp_dir="${4}"
    assert-args name hint transcript tmp_dir @RET

    local label
    label="$(h-agent-session-label-of "${transcript}")"

    if ! test -e "${transcript}" ; then
        h-agent-view-fail "${name}" "${tmp_dir}" "this session has no transcript on disk yet"
        return 1
    fi

    local title
    title="$(h-agent-session-name "${transcript}")" || title=''
    title="${title:-${transcript:t:r}}"

    h-agent-view-banner "${name}" notice 600 0 "**${label}** → org: *${title}*   (${hint} again cancels)"

    #: Named after the session, so the emacs buffer is recognizable; the id
    #: disambiguates two sessions sharing a name.
    local out_file="${tmp_dir}/${title}.org"
    if test -e "${out_file}" ; then
        out_file="${tmp_dir}/${title}-${${transcript:t:r}[1,8]}.org"
    fi

    #: `local' is dynamically scoped, so the renderer several calls down sees
    #: this without anything being exported. An explicit setting wins.
    local agent_session_render_jobs="${agent_session_render_jobs:-${claude_code_session_render_jobs:-$(h-agent-view-jobs-share)}}"

    if ! h-agent-session-to-org "${transcript}" "${out_file}" ; then
        h-agent-view-fail "${name}" "${tmp_dir}" "conversion failed: ${title}"
        return 1
    fi

    #: Hand the bookkeeping back before handing the file over. From here the
    #: directory has to survive: emacs will be holding the file open, and a
    #: cancel landing in this window would otherwise delete it underneath, which
    #: leaves a buffer pointing at nothing. The canceller reads an empty value
    #: and [agfi:h-agent-view-rm-tmp] returns early on it; on success the
    #: directory stays regardless, which is what it always did.
    silent tmux set-option -u -t "${name}" '@ccv_tmp' || true

    if ! emc-open "${out_file}" ; then
        h-agent-view-fail "${name}" '' "emacs did not open ${out_file:t}"
        return 1
    fi

    h-agent-view-dismiss "${name}"
}

function h-agent-view-fail {
    #: Reports a failed conversion the only ways a detached job can be heard --
    #: a red band and a notification -- and removes its temp directory, which
    #: may hold a truncated .org. An empty tmp dir means there is nothing to
    #: remove.
    #: Usage: h-agent-view-fail <name> <tmp dir or empty> <reason>
    ##
    local name="${1}" tmp_dir="${2}" reason="${3}"

    #: `funcstack[2]' rather than the hotkey's name, which was hardcoded here
    #: when the hotkey was the only caller.
    ecerr "${funcstack[2]:-$0}: ${reason}"
    h-agent-view-banner "${name}" crit 8 0.35 "**Agent session**: ${reason}"
    silence notif "Agent session: ${reason}"

    h-agent-view-rm-tmp "${tmp_dir}"
    return 1
}

function h-agent-view-rm-tmp {
    #: Removes a conversion's temp directory, and only something that looks like
    #: one: a `gmktemp --directory' path, `tmp.XXXXXX' under a temp root. A
    #: bookkeeping value that is empty or odd is left alone rather than fed to
    #: `rm -rf'.
    ##
    local d="${1}"
    test -n "${d}" || return 0
    [[ "${d}" == /*/* && "${d:t}" == tmp.* ]] || return 0

    command rm -rf -- "${d}"
}

function h-agent-view-cancel {
    #: Cancels a tab's running conversion. Bound to the same key that started
    #: it: [agfi:h-agent-view-session-focused] calls this when the tab's
    #: session is alive.
    #:
    #: Order matters, twice. The bookkeeping is read before the kill, since
    #: killing the session discards its options. And the band is changed before
    #: the kill, so that nothing the death triggers can repaint it -- the same
    #: rule the FIM canceller follows. The tree kill is
    #: `tmux-session-processes-kill', which recurses through the renderer's
    #: pandoc children; the cleanup is here rather than in the job, because a
    #: killed process is not guaranteed to run an `always' block.
    #: Usage: h-agent-view-cancel <tab-key>
    ##
    local key="${1}"
    assert-args key @RET

    local name
    name="$(h-agent-view-name-of "${key}")" @RET

    local tmp_dir
    tmp_dir="$(tmux show-options -qv -t "${name}" '@ccv_tmp' 2>/dev/null)" || tmp_dir=''

    h-agent-view-banner "${name}" warn 2 0 "**Agent session** → org: cancelled"

    silent tmux-session-processes-kill "${name}"

    h-agent-view-rm-tmp "${tmp_dir}"
}
##
#: Live sessions, and the pickers over them
##
function h-agent-session-live-pairs {
    #: `<kitty-window-id> <TAB> <transcript> <TAB> <agent> <TAB> <name>' for
    #: every kitty window showing an agent session, one per line.
    #:
    #: Each window goes through [agfi:h-agent-session-of-kitty-window], so this
    #: sees sessions attached through tmux or an agent view, not only ones that
    #: registered themselves. One live listing serves every window, through the
    #: cache the resolver honours.
    ##
    ensure-cmd kitty jq @RET

    local sock
    sock="$(h-agent-session-kitty-socket)" @RET

    local kpid
    kpid="$(kitty-socket-pid "${sock}")" @RET

    local ls_json
    ls_json="$(kitty @ --to "${sock}" ls)" @RET

    local agent_session_live_list_cache="${agent_session_live_list_cache:-$(h-agent-session-live-list)}"
    #: Parsed once here, so resolving twenty windows costs one `jq' rather
    #: than one per window per field.
    local agent_session_windows_cache="${agent_session_windows_cache:-$(h-agent-session-windows "${ls_json}")}"
    #: And the hooks' record for every tmux session, one listing rather than a
    #: `show-option' per window.
    local agent_session_tmux_identities="${agent_session_tmux_identities:-$(h-agent-session-tmux-identities)}"
    local agent_session_tmux_clients="${agent_session_tmux_clients:-$(h-agent-session-tmux-clients)}"

    local -a ids
    ids=( ${(f)"$(ec "${agent_session_windows_cache}" | command cut -f1)"} )
    (( ${#ids} )) || return 1

    #: Two maps built once, so the loop below forks for nothing but the
    #: resolution itself. A command substitution is a fork, and the name and
    #: the agent used to cost one each per window: twenty windows, forty forks,
    #: a third of what this function spent.
    local -A names
    local row
    local -a nf
    for row in ${(f)agent_session_live_list_cache} ; do
        nf=( "${(@ps:\t:)row}" )
        test -n "${nf[5]}" || continue
        names[${nf[5]}]="${nf[3]:--}"
    done

    local -a root_agents
    root_agents=( ${(f)"$(h-agent-session-root-agents)"} )

    local id transcript agent
    for id in "${ids[@]}" ; do
        test -n "${id}" || continue

        transcript="$(h-agent-session-of-kitty-window "${ls_json}" "${id}" "${kpid}")" || continue
        h-agent-session-agent-in-roots "${transcript}" "${root_agents[@]}"

        printf '%s\t%s\t%s\t%s\n' "${id}" "${transcript}" "${REPLY}" "${names[${transcript}]:--}"
    done
}

function h-agent-session-root-agents {
    #: `<root><TAB><agent>' for every root of every agent [agfi:h-agents]
    #: lists. What [agfi:h-agent-session-agent-in-roots] matches against, so a
    #: loop over transcripts can name their agents without a fork each.
    ##
    local agent root
    for agent in ${(f)"$(h-agents)"} ; do
        for root in ${(f)"$(h-agent-session-call "${agent}" roots 2>/dev/null)"} ; do
            test -n "${root}" || continue
            print -r -- "${root%/}"$'\t'"${agent}"
        done
    done
}

function h-agent-session-agent-in-roots {
    #: Sets `REPLY' to the agent whose store holds the transcript $1, or to
    #: `-'. The remaining arguments are [agfi:h-agent-session-root-agents]
    #: rows.
    #:
    #: The same answer [agfi:h-agent-session-agent-of] gives for a path inside a
    #: store, without the fork a command substitution costs -- zsh has no
    #: namerefs, so `REPLY' is how a helper hands a value back in a loop. A
    #: transcript outside every store is left to the sniffing path, which
    #: nothing in a picker needs.
    #: Usage: h-agent-session-agent-in-roots <transcript> <root<TAB>agent>...
    ##
    local transcript="${1}"
    shift

    local row
    for row in "$@" ; do
        if [[ "${transcript}" == "${row%%$'\t'*}"/* ]] ; then
            REPLY="${row#*$'\t'}"
            return 0
        fi
    done

    REPLY='-'
    return 1
}

function h-agent-session-all-pairs {
    #: [agfi:h-agent-session-live-pairs] over every live session: the window
    #: column reads `-' for a session no kitty window is showing. Windows first,
    #: so a session showing in one is listed under it.
    ##
    local agent_session_live_list_cache="${agent_session_live_list_cache:-$(h-agent-session-live-list)}"

    local in_windows
    in_windows="$(h-agent-session-live-pairs 2>/dev/null)" || in_windows=''
    test -n "${in_windows}" && ec "${in_windows}"

    local -a live f
    live=( ${(f)agent_session_live_list_cache} )

    local -a root_agents
    root_agents=( ${(f)"$(h-agent-session-root-agents)"} )

    local row t
    for row in "${live[@]}" ; do
        f=( "${(@ps:\t:)row}" )
        t="${f[5]}"
        test -n "${t}" && test -e "${t}" || continue
        [[ "${in_windows}" == *$'\t'"${t}"$'\t'* ]] && continue

        h-agent-session-agent-in-roots "${t}" "${root_agents[@]}"
        printf -- '-\t%s\t%s\t%s\n' "${t}" "${REPLY}" "${f[3]}"
    done
}

function h-agent-session-pick-rows {
    #: [agfi:h-agent-session-live-rows] over every live session, showing in a
    #: kitty window or not. What the overlay picker offers.
    ##
    agent_session_live_rows_scope=all h-agent-session-live-rows
}

function h-agent-session-live-rows {
    #: The rows [agfi:agent-session-live-fz] offers, tab separated: window id,
    #: transcript, agent, label (`<glyph> w<id>  <name>`, or the bare name for
    #: a session in no window), profile (Claude's config home; `-' otherwise),
    #: last activity, relative path, snippet. The first three are for the
    #: caller, the rest for the person choosing.
    ##
    #: `windows' (default): sessions showing in a kitty window, from
    #: [agfi:h-agent-session-live-pairs]. `all': every live session, from
    #: [agfi:h-agent-session-all-pairs], `-' in the window column when none
    #: shows it.
    local scope="${agent_session_live_rows_scope:-${claude_code_session_live_rows_scope:-windows}}"

    local pairs
    if [[ "${scope}" == all ]] ; then
        pairs="$(h-agent-session-all-pairs)" @RET
    else
        pairs="$(h-agent-session-live-pairs)" @RET
    fi
    if test -z "${pairs}" ; then
        ecerr "$0: no live agent session with a transcript to show"
        return 1
    fi

    h-agent-session-dep @RET

    #: One `list` per agent and then a join, rather than a metadata call per
    #: row: `name` alone costs ~230ms on a large transcript. Anything the join
    #: misses still gets a row, just a barer one.
    #:
    #: `-only` names the transcripts these rows are about, so the corpus is not
    #: walked at all: annotating twenty live sessions used to mean listing
    #: every session on disk, which for Codex is fifteen hundred rollouts and
    #: 280ms of the picker's startup. With the paths in hand it is 6ms.
    #:
    #: `list` carries a name of its own, which is the same name by another route
    #: -- read out of the transcript rather than asked of the agent -- so it
    #: stands in when the live listing has none.
    #: `t', not `path': zsh ties `path' to `PATH', so a loop over transcripts
    #: named `path' replaces PATH with the transcript it is holding and every
    #: command after it is not found.
    local agent meta='' glyphs='' t
    local -a roots only transcripts
    transcripts=( ${(f)"$(ec "${pairs}" | command cut -f2)"} )

    for agent in ${(f)"$(h-agents)"} ; do
        glyphs+="${agent}"$'\t'"$(h-agent-field "${agent}" glyph)"$'\n'
        roots=( ${(f)"$(h-agent-session-call "${agent}" roots 2>/dev/null)"} ) || continue
        (( ${#roots} )) || continue

        only=()
        for t in "${transcripts[@]}" ; do
            test -n "${t}" || continue
            h-agent-session-call "${agent}" owns-p "${t}" || continue
            only+=( -only "${t}" )
        done
        (( ${#only} )) || continue

        meta+="$(agent_session "${agent}" list "${only[@]}" "${roots[@]}" 2>/dev/null)"$'\n'
    done

    ec "${pairs}" |
        gawk -F'\t' -v OFS='\t' '
            FILENAME == ARGV[1] { glyph[$1] = $2 ; next }
            FILENAME == ARGV[2] { when[$2] = $3 ; nm[$2] = $4 ; rel[$2] = $5 ; snip[$2] = $6 ; next }
            {
                path = $2
                agent = $3

                #: For Claude, the profile is the config home the transcript
                #: sits under -- .claude, .claude-work -- which is the same
                #: label `list` puts on its own relative paths.
                profile = "-"
                if (agent == "claude") {
                    profile = path
                    sub(/\/projects\/.*$/, "", profile)
                    sub(/^.*\//, "", profile)
                }

                name = ($4 == "-" && nm[path]) ? nm[path] : $4
                label = ($1 == "-") ? name : ("w" $1 "  " name)
                if (glyph[agent] != "") label = glyph[agent] " " label

                print $1, path, agent, label, profile, \
                    (when[path] ? when[path] : "?"), \
                    (rel[path] ? rel[path] : path), \
                    snip[path]
            }
        ' <(ec "${glyphs}") <(ec "${meta}") -
}

function h-agent-session-preview-cmd {
    #: The fzf `--preview' command for a session row, shell-quoted and ready to
    #: have a field placeholder appended:
    #:
    #:     --preview "$(h-agent-session-preview-cmd '{3}') {2}"
    #:
    #: The agent goes in unquoted, so fzf's `{3}' placeholder can stand there
    #: and be filled in per row; a picker of one agent bakes the name instead.
    #:
    #: An absolute path to the binary and nothing else in the command, because
    #: fzf runs this in its own dash once per cursor move and every layer in
    #: between costs real time. The shell version of the preview forked for
    #: [agfi:h-color-p-override] on each of ~25 colour calls (122ms of 277ms),
    #: shelled out to `tail' and `jq', and then reached us through the brish
    #: garden -- about 600ms a keystroke, end to end.
    #:
    #: Those component figures were taken on a loaded machine and do not all
    #: reproduce: the garden round trip measured ~380ms then and measures ~60ms
    #: idle. The 600ms total and the 15ms replacement were both measured the
    #: same way, so the comparison holds even though the parts have shrunk.
    #: Exec'ing the binary is 15ms, most of it process startup rather than
    #: work: `agent_session --help' alone is 10.7ms and the dash spawn 4.3ms,
    #: so the scan is about 5ms and does not grow with the transcript --- a
    #: 26MB one measures 16.1ms, since only the tail is read.
    #:
    #: The knobs are baked in here rather than read by the preview, since a zsh
    #: global cannot reach a process fzf spawns on its own.
    #: Usage: h-agent-session-preview-cmd [agent or {3}]
    ##
    local agent="${1:-{3\}}"
    if [[ "${agent}" != [[:alnum:]{}]## ]] ; then
        ecerr "$0: refusing to splice into a shell command: ${agent}"
        return 1
    fi

    local bytes="${agent_session_preview_bytes:-${claude_code_session_preview_bytes}}"
    local color_p="${agent_session_preview_color_p:-${claude_code_session_preview_color_p:-y}}"

    h-agent-session-dep @RET

    local -a cmd
    cmd=( preview )
    test -n "${bytes}" && cmd+=( "-bytes=${bytes}" )
    bool "${color_p}" || cmd+=( '-color=false' )

    ec "$(gquote "${commands[agent_session]:-agent_session}") ${agent} $(gquote "${cmd[@]}")"
}

function h-agent-session-open-cmd {
    #: The fzf `--bind alt-enter' command for a session row, shell-quoted and
    #: ready to have a field placeholder appended:
    #:
    #:     --bind "alt-enter:execute-silent($(h-agent-session-open-cmd) {2})"
    #:
    #: Fire-and-forget, through `brishzb.dash' rather than `brishzq.zsh'.
    #: `execute-silent' blocks fzf until the command returns, and brishzq waits
    #: for the whole call -- 250ms measured, most of it not the garden hop
    #: (~60ms) but [agfi:h-agent-view-launch]'s own foreground work: the
    #: Hammerspoon band call, `gmktemp' and `tmuxnewsh2'. brishzb posts
    #: `{ cmd } &>/dev/null &' and returns as soon as the garden forks: 20ms.
    #:
    #: Losing stdout and the exit status costs nothing here, because the work is
    #: asynchronous either way -- every failure already reports through the band
    #: and a notification, in [agfi:h-agent-view-fail].
    #:
    #: brishzb splices its arguments into JSON unquoted, which brishzq would
    #: not, and that is safe here rather than by luck: fzf shell-quotes `{2}'
    #: with single quotes, and a transcript path cannot contain a `"' or a `\'
    #: --- each agent names its files after ids and encoded directories.
    #:
    #: The absolute path comes from `$commands', so nothing depends on whatever
    #: PATH fzf happens to have.
    #:
    #: One trap for a future caller: fzf's `--expect' beats `--bind' for the
    #: same key whatever the order, so an `--expect=alt-enter' anywhere in a
    #: picker's `fz_opts' silently disables this and makes alt+enter accept
    #: instead, returning the literal string `alt-enter' as the choice. The
    #: idiom is in use nearby, in [agfi:h-grep-output-to-fz] and [agfi:rgf_].
    ##
    local brishzb="${commands[brishzb.dash]:-brishzb.dash}"

    gquote "${brishzb}" agent-view-session-toggle
}

#: What the pickers put in their `--header', so the binding is discoverable at
#: all. A person who does not know it exists will never press it.
#: `${x:-...}' because =zshlang/load-others.zsh= sources `personal/' before
#: `auto-load/', so a bare assignment would silently overwrite an override.
#: Names the format, since Enter does something different in each picker ---
#: resume, raw transcript, markdown --- while alt+enter is always org.
typeset -g agent_session_fz_header="${agent_session_fz_header:-alt+enter: → org in emacs, in the background (again cancels)}"

function h-agent-session-fz-parts {
    #: The three things a picker outside zshlang needs from us, one per line:
    #: the preview command, the alt+enter command, and the header. For
    #: =zshlang/wrappers/agent-session-pick.zsh=, which runs under `zsh -f' in
    #: a kitty overlay and can only reach us through the garden.
    #:
    #: One call rather than three. Each garden round trip is ~380ms, and the
    #: overlay pays them at startup while a person waits. None of the three
    #: values can contain a newline, so a line per value needs no quoting.
    ##
    h-agent-session-preview-cmd '{3}' @RET
    h-agent-session-open-cmd @RET
    ec "${agent_session_fz_header}"
}

function h-agent-session-preview {
    #: The fzf preview body for one session: what it is called, what it was
    #: running as, where, when it last moved, and what was last asked of it.
    #:
    #: A wrapper over `agent_session <agent> preview', kept as the convenient
    #: way to see the preview for a session by hand. The pickers themselves do
    #: not come through here; see [agfi:h-agent-session-preview-cmd].
    ##
    local transcript="${1}"
    assert-args transcript @RET

    local agent cmd
    agent="$(h-agent-session-agent-of "${transcript}")" @RET
    cmd="$(h-agent-session-preview-cmd "${agent}")" @RET

    #: `cmd' is already shell-quoted, for fzf's benefit, so the path is quoted
    #: to match rather than passed as an argument.
    eval "${cmd} ${(qq)transcript}"
}

function agent-session-live-fz {
    #: Picks among the agent sessions currently live in a kitty window, and
    #: prints `<kitty-window-id> <TAB> <transcript path>` for each choice.
    #:
    #: Multi-select by default, since the callers want to act on several tabs at
    #: once; set agent_session_live_fz_no_multi_p=y for one.
    #: agent_session_live_fz_extra_rows prepends synthetic choices (in the row
    #: layout of [agfi:h-agent-session-live-rows]), which is how
    #: [agfi:h-claude-code-usage-type-continue-target-fz] offers "frontmost"
    #: alongside the real sessions.
    ##
    local rows
    rows="$(h-agent-session-live-rows)" @RET

    ensure-array agent_session_live_fz_extra_rows claude_code_session_live_fz_extra_rows
    local -a extra_rows
    extra_rows=("${agent_session_live_fz_extra_rows[@]}" "${claude_code_session_live_fz_extra_rows[@]}")
    if (( ${#extra_rows} )) ; then
        rows="${(F)extra_rows}"$'\n'"${rows}"
    fi

    ensure-array agent_session_live_fz_opts claude_code_session_live_fz_opts
    local fz_opts=("${agent_session_live_fz_opts[@]}" "${claude_code_session_live_fz_opts[@]}")

    local multi=multi
    if bool "${agent_session_live_fz_no_multi_p:-${claude_code_session_live_fz_no_multi_p:-n}}" ; then
        multi=no-multi
    fi

    #: `{2}' under `--multi' is the highlighted row, not the selection, which
    #: is what alt+enter wants: it acts on the one row you are looking at.
    local selected
    selected="$(ec "${rows}" | h-agent-session-fz "${multi}" "${fz_opts[@]}")" @RET

    test -n "${selected}" || return 1

    #: The display columns were only ever for the person choosing.
    ec "${selected}" | command cut -f1,2
}
##
#: Locating and resuming
##
function h-agent-session-resolve {
    #: A transcript path from either a path or a session id (a unique prefix of
    #: one will do). An id is looked up by every agent in [agfi:h-agents], or by
    #: the one named. Several matches are an error rather than a guess.
    #: Usage: h-agent-session-resolve [agent] <path|id>
    ##
    local agent='' input="${1}"
    if (( $# >= 2 )) ; then
        agent="${1}" input="${2}"
    fi
    assert-args input @RET

    if [[ "${input}" == */* ]] || test -e "${input}" ; then
        if ! test -e "${input}" ; then
            ecerr "$0: transcript does not exist: ${input}"
            return 1
        fi
        ec "${input:a}"
        return 0
    fi

    local -a agents hits
    if test -n "${agent}" ; then
        agents=("${agent}")
    else
        agents=( ${(f)"$(h-agents)"} ) @TRET
    fi

    local a out
    for a in "${agents[@]}" ; do
        out="$(h-agent-session-call "${a}" resolve "${input}" 2>/dev/null)" || continue
        test -n "${out}" && hits+=( ${(f)out} )
    done

    if (( ${#hits} == 0 )) ; then
        ecerr "$0: no session matches '${input}' (agents: ${(j:, :)agents})"
        return 1
    elif (( ${#hits} > 1 )) ; then
        ecerr "$0: '${input}' is ambiguous:"
        ecerr "  ${(pj:\n  :)hits}"
        return 1
    fi

    ec "${hits[1]}"
}

function agent-session-current-agent {
    #: Which agent spawned this shell, or recorded itself on this tmux session.
    ##
    local agent
    if agent="$(ai-agent-name 2>/dev/null)" ; then
        ec "${agent}"
        return 0
    fi

    local identity
    identity="$(agent-tmux-identity-get 2>/dev/null)" || return 1
    ec "${identity%%$'\t'*}"
}

function agent-session-current-file {
    #: The transcript of the agent session that spawned this shell, whichever
    #: agent it is: the id comes from the environment the agent exports (its
    #: adapter's `current-id'), else from the record the hooks leave on the tmux
    #: session (=agent-tmux.zsh=).
    ##
    local agent
    if ! agent="$(agent-session-current-agent)" ; then
        ecerr "$0: not inside an agent session"
        return 1
    fi

    local id
    if ! id="$(h-agent-session-call "${agent}" current-id 2>/dev/null)" ; then
        local identity
        identity="$(agent-tmux-identity-get)" @RET
        local -a f
        f=( "${(@ps:\t:)identity}" )
        if test -n "${f[3]}" && test -e "${f[3]}" ; then
            ec "${f[3]}"
            return 0
        fi
        id="${f[2]}"
    fi
    test -n "${id}" || return 1

    h-agent-session-resolve "${agent}" "${id}"
}

function agent-session-current-name {
    #: The name of the agent session that spawned this shell; see
    #: [agfi:h-agent-session-name] for which of its names wins.
    ##
    local file
    file="$(agent-session-current-file)" @RET

    h-agent-session-name "${file}"
}

function h-agent-session-dir {
    #: The directory a session was working in. Nothing here is inferred if it
    #: can be helped: every agent records the cwd in its transcript, and
    #: `agent_session <agent> meta' prints it as the third field for all three,
    #: so the exact directory is available for the asking.
    #:
    #: Two fallbacks stand behind that, in order. Claude Code names its project
    #: directory after the directory a session started in, with every
    #: non-alphanumeric character replaced by a dash -- lossy, since
    #: `-Users-evar-my-dir' could be `/Users/evar/my-dir' or
    #: `/Users/evar/my/dir', so the inversion is accepted only when the result
    #: really is a directory. Then whatever the caller offered.
    #: Usage: h-agent-session-dir <transcript> [agent] [fallback]
    ##
    local transcript="${1}" agent="${2}" fallback="${3}"
    assert-args transcript @RET

    if test -z "${agent}" ; then
        agent="$(h-agent-session-agent-of "${transcript}" 2>/dev/null)" || agent=''
    fi

    if test -n "${agent}" ; then
        local meta cwd
        if meta="$(agent_session "${agent}" meta "${transcript}" 2>/dev/null)" ; then
            cwd="${${(@ps:\t:)meta}[3]}"
            if test -n "${cwd}" && test -d "${cwd}" ; then
                ec "${cwd}"
                return 0
            fi
        fi
    fi

    if [[ "${agent}" == claude ]] ; then
        local candidate="${${transcript:h:t}//-//}"
        if test -d "${candidate}" ; then
            ec "${candidate}"
            return 0
        fi
    fi

    test -n "${fallback}" || return 1
    ec "${fallback}"
}

function h-agent-session-resume-run {
    #: Runs an agent's resume command in the session's own directory. None of
    #: the three agents does this for you: [agfi:claude-code-session-resume]
    #: used to only *warn* that "tools will run in ${PWD}", and Codex and agy
    #: did not even warn. Resuming a session into a directory it knows nothing
    #: about is the kind of thing you notice three tool calls later, when a
    #: relative path or a project instruction file has quietly gone missing.
    #:
    #: `agent_session_resume_cd_p=n' keeps the old behaviour and restores the
    #: warning with it, for resuming a session deliberately somewhere else.
    #:
    #: In a subshell, so an interactive caller is not silently left in another
    #: directory once the session exits.
    #: Usage: h-agent-session-resume-run <transcript> <command...>
    ##
    local transcript="${1}"
    shift
    assert-args transcript @RET
    (( $# )) || return 1

    local dir=''
    dir="$(h-agent-session-dir "${transcript}" 2>/dev/null)" || dir=''

    if test -z "${dir}" || [[ "${dir:A}" == "${PWD:A}" ]] ; then
        "$@"
        return $?
    fi

    if ! bool "${agent_session_resume_cd_p:-y}" ; then
        ecerr "$0: warning: this session was working in ${dir/#${HOME}/~}; tools will run in ${PWD/#${HOME}/~}"
        "$@"
        return $?
    fi

    ecgray "$0: resuming in ${dir/#${HOME}/~}"
    ( builtin cd -q -- "${dir}" && "$@" )
}

function agent-session-resume {
    #: Resumes a session in its own agent: the adapter's `resume' verb runs the
    #: launcher with whatever "continue this one" spelling the agent has.
    #: Anything after the first argument goes to the launcher.
    #: Usage: agent-session-resume <transcript|id> [agent args...]
    ##
    local session="${1}"
    shift
    assert-args session @RET

    local transcript agent
    transcript="$(h-agent-session-resolve "${session}")" @RET
    agent="$(h-agent-session-agent-of "${transcript}")" @RET

    h-agent-session-call "${agent}" resume "${transcript}" "$@"
    local ret=$?
    if (( ret == 2 )) ; then
        ecerr "$0: ${agent} has no resume verb"
    fi
    return ret
}

function agent-session-resume-fz {
    #: Picks a session with [agfi:h-agent-session-select-fz] and hands it to
    #: [agfi:agent-session-resume].
    #: Usage: agent-session-resume-fz [agent args...]
    ##
    #: `project' (default), or `all' to choose from every project's sessions
    #: rather than this directory's. The picker's own knob under a name of our
    #: own, so the `-all-fz' variant can set it the way the viewers do.
    local scope="${agent_session_resume_scope:-${claude_code_session_resume_scope:-project}}"
    local agent_session_fz_scope="${scope}"

    local transcript
    transcript="$(h-agent-session-select-fz)" @RET

    agent-session-resume "${transcript}" "$@"
}
#: Same, but selects from the sessions of all projects.
aliasfn agent-session-resume-all-fz agent_session_resume_scope=all agent-session-resume-fz
##
function agent-session-selftest {
    #: Runs the renderer's Go tests, then checks its parallel pandoc path
    #: against a single pandoc run over every local Claude Code transcript.
    ##
    ensure-cmd go pandoc @RET

    local dir="${NIGHTDIR}/golang/agent_session"
    #: Every profile's transcripts, one run each: the parity check takes a
    #: single directory, and picking just one of them would quietly shrink the
    #: corpus to whichever sorted first.
    local -a corpus_dirs
    corpus_dirs=("${(@f)$(h-agent-session-call claude roots)}") @TRET

    pushf "${dir}" && {
        assert go test -count=1 ./... @RET

        local corpus
        for corpus in "${corpus_dirs[@]}" ; do
            ecgray "$0: parity over ${corpus/#${HOME}/~}"
            AGENT_SESSION_CORPUS="${corpus}" assert go test -count=1 -v -run Parity ./... @RET
        done
    } always { popf }
}
