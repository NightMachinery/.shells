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
    #: The session's own name (Claude Code's slug, e.g.
    #: `sharded-bouncing-clarke`), falling back to its UUID for sessions
    #: predating the feature. Sanitized for use as a filename.
    ##
    local input="${1}"

    h-claude-code-session-dep @RET

    local slug
    slug="$(claude_session slug "${input}")" @RET
    slug="${slug//[^A-Za-z0-9._-]/-}"

    if test -z "${slug}" ; then
        ec "${input:t:r}"
    else
        ec "${slug}"
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
    #: see =docs/claude_session.md=.
    {
        h-claude-code-session-title org "${input}" @RET
        h-claude-code-session-render org-pandoc "${input}" @RET
    } > "${out}"
}
aliasfn h-claude-code-session-to-org h-claude-code-session-to-org-pandoc

function h-claude-code-session-select-fz {
    #: Interactively selects a Claude Code session `.jsonl` file and
    #: prints its path.
    ##
    local scope="${claude_code_view_session_fz_scope:-project}"
    local projects_dir="${claude_code_view_session_fz_projects_dir:-${HOME}/.claude/projects}"
    ensure-array claude_code_view_session_fz_fz_opts
    local fz_opts=("${claude_code_view_session_fz_fz_opts[@]}")

    local sessions_dir="${projects_dir}"
    if [[ "${scope}" != "all" ]] ; then
        local project_dir_name="${${PWD//\//-}//./-}"
        sessions_dir="${projects_dir}/${project_dir_name}"
    fi

    if ! test -d "${sessions_dir}" ; then
        ecerr "$0: sessions directory does not exist: ${sessions_dir}"
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

    local lines
    lines="$(claude_session list "${list_args[@]}" "${sessions_dir}")" @RET

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

function h-claude-code-view-session-fz {
    #: Interactively selects a Claude Code session, converts it using the
    #: given converter function, and opens the result in emacs.
    ##
    local converter="${1}"
    local ext="${2}"

    local session_file
    session_file="$(h-claude-code-session-select-fz)" @RET

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

function claude-session-selftest {
    #: Runs the renderer's Go tests, then checks its parallel pandoc path
    #: against a single pandoc run over every local session transcript.
    ##
    ensure-cmd go pandoc @RET

    local dir="${NIGHTDIR}/golang/claude_session"
    local corpus="${claude_code_view_session_fz_projects_dir:-${HOME}/.claude/projects}"

    pushf "${dir}" && {
        assert go test -count=1 ./... @RET
        CLAUDE_SESSION_CORPUS="${corpus}" assert go test -count=1 -v -run Parity ./... @RET
    } always { popf }
}
##
