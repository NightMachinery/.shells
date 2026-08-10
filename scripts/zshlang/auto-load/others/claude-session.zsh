##
#: Rendering and session scanning live in =golang/claude_session.go=; these
#: are thin wrappers around it. See =docs/claude_session.md=.
##
function h-claude-code-session-render {
    #: Renders a Claude Code session `.jsonl` to stdout.
    #: Usage: h-claude-code-session-render <format> <input>
    ##
    local format="${1}" input="${2}"

    local render_args=("-format=${format}")
    local max_lines="${claude_code_session_max_block_lines:-0}"
    render_args+=("-max-block-lines=${max_lines}")
    if bool "${claude_code_session_diff_p:-y}" ; then
        render_args+=(-diff)
    else
        render_args+=(-diff=false)
    fi

    assert claude_session.go render "${render_args[@]}" "${input}" @RET
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
        ec "# Claude Code Session ${input:t:r}"
        ec
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
        ec "#+TITLE: Claude Code Session ${input:t:r}"
        ec
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
        ec "#+TITLE: Claude Code Session ${input:t:r}"
        ec
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

    #: `epoch<TAB>path<TAB>local time<TAB>relative path<TAB>snippet`, newest
    #: first. The time is the last message's, not the file's mtime; see
    #: =docs/claude_session.md=.
    local lines
    lines="$(claude_session.go list "${sessions_dir}")" @RET

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

    local out_file="${tmp_dir}/${session_file:t:r}.${ext}"
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
##
