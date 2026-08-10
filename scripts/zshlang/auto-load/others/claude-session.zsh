##
function h-claude-code-session-jq-lib {
    #: Prints jq definitions shared by the session renderers.
    #:
    #: `human_timestamp` turns the ISO-8601 UTC timestamps that Claude Code
    #: writes (e.g. `2026-08-10T03:09:30.631Z`) into inactive org timestamps
    #: in the local timezone (e.g. `[2026-08-10 Mon 05:09]`).
    ##
    cat <<'EOF'
def human_timestamp:
  if type == "string" then
    (try (sub("\\.[0-9]+Z$"; "Z") | fromdateiso8601
          | strflocaltime("[%Y-%m-%d %a %H:%M]"))
     catch .)
  else "" end;
EOF
}

function h-claude-code-session-org-jq-program {
    #: Prints the jq program that renders a Claude Code session `.jsonl`
    #: directly as org-mode.
    ##
    h-claude-code-session-jq-lib @RET

    cat <<'EOF'
def esc_block:
  tostring
  | split("\n")
  | map(if (startswith("*") or startswith("#+")) then "," + . else . end)
  | join("\n");

def esc_text:
  tostring
  | split("\n")
  | map(if (startswith("*") or startswith("#+")) then " " + . else . end)
  | join("\n");

def in_example:
  "#+begin_example\n" + esc_block + "\n#+end_example";

def render_tool_input:
  if .name == "Bash" then (.input.command // (.input | tojson))
  else (.input | tojson)
  end;

def render_block:
  if .type == "text" then (.text | esc_text)
  elif .type == "thinking" then ("** Thinking\n" + (.thinking | in_example))
  elif .type == "tool_use" then
    ("** Tool Use: " + (.name // "?") + "\n" + (render_tool_input | in_example))
  elif .type == "tool_result" then
    ("** Tool Result"
     + (if .is_error == true then " (error)" else "" end)
     + "\n"
     + (((.content // "")
         | if type == "array"
           then (map(if type == "object" then (.text // tojson) else tostring end)
                 | join("\n"))
           else . end)
        | in_example))
  else empty
  end;

def role: (.type[0:1] | ascii_upcase) + .type[1:];

select(type == "object")
| select(.type == "user" or .type == "assistant")
| select(.isMeta != true)
| "* " + role
  + (if (.timestamp | type) == "string" then " " + (.timestamp | human_timestamp) else "" end)
  + "\n"
  + ((.message.content // [])
     | if type == "string" then [{type: "text", text: .}] else . end
     | map(render_block)
     | join("\n\n"))
  + "\n"
EOF
}

function h-claude-code-session-to-org-jq {
    #: Converts a Claude Code session `.jsonl` file into an org-mode file.
    ##
    local input="${1}"
    local out="${2:-${input:r}.org}"

    if ! test -e "${input}" ; then
        ecerr "$0: input file does not exist: ${input}"
        return 1
    fi

    local jq_program
    jq_program="$(h-claude-code-session-org-jq-program)" @TRET

    {
        ec "#+TITLE: Claude Code Session ${input:t:r}"
        ec
        assert jq --raw-output "${jq_program}" "${input}" @RET
    } > "${out}"
}

function h-claude-code-session-md-jq-program {
    #: Prints the jq program that renders a Claude Code session `.jsonl`
    #: as markdown.
    ##
    h-claude-code-session-jq-lib @RET

    cat <<'EOF'
def fence_for:
  ([scan("`+") | length] | max // 0) as $m
  | (if $m + 1 < 3 then 3 else $m + 1 end) as $len
  | ("`" * $len);

def fenced($lang):
  tostring
  | . as $s
  | ($s | fence_for) as $f
  | $f + $lang + "\n" + $s + "\n" + $f;

def render_tool_input:
  if .name == "Bash" then ((.input.command // (.input | tojson)) | fenced("zsh"))
  else ((.input | tojson) | fenced("json"))
  end;

def render_block:
  if .type == "text" then .text
  elif .type == "thinking" then ("## Thinking\n\n" + (.thinking | fenced("")))
  elif .type == "tool_use" then
    ("## Tool Use: " + (.name // "?") + "\n\n" + render_tool_input)
  elif .type == "tool_result" then
    ("## Tool Result"
     + (if .is_error == true then " (error)" else "" end)
     + "\n\n"
     + (((.content // "")
         | if type == "array"
           then (map(if type == "object" then (.text // tojson) else tostring end)
                 | join("\n"))
           else . end)
        | fenced("")))
  else empty
  end;

def role: (.type[0:1] | ascii_upcase) + .type[1:];

select(type == "object")
| select(.type == "user" or .type == "assistant")
| select(.isMeta != true)
| "# " + role
  + (if (.timestamp | type) == "string" then " " + (.timestamp | human_timestamp) else "" end)
  + "\n\n"
  + ((.message.content // [])
     | if type == "string" then [{type: "text", text: .}] else . end
     | map(render_block)
     | join("\n\n"))
  + "\n"
EOF
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

    local jq_program
    jq_program="$(h-claude-code-session-md-jq-program)" @TRET

    {
        ec "# Claude Code Session ${input:t:r}"
        ec
        assert jq --raw-output "${jq_program}" "${input}" @RET
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

    local jq_program
    jq_program="$(h-claude-code-session-md-jq-program)" @TRET

    local markdown
    markdown="$(jq --raw-output "${jq_program}" "${input}")" @RET

    {
        ec "#+TITLE: Claude Code Session ${input:t:r}"
        ec
        ec "${markdown}" | assert pandoc --from=gfm --to=org --wrap=none @RET
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

    zmodload -F zsh/stat b:zstat @RET

    local sessions_dir="${projects_dir}"
    if [[ "${scope}" != "all" ]] ; then
        local project_dir_name="${${PWD//\//-}//./-}"
        sessions_dir="${projects_dir}/${project_dir_name}"
    fi

    if ! test -d "${sessions_dir}" ; then
        ecerr "$0: sessions directory does not exist: ${sessions_dir}"
        return 1
    fi

    local -a session_files
    session_files=("${sessions_dir}"/**/*.jsonl(N.om))
    if (( ${#session_files} == 0 )) ; then
        ecerr "$0: no session files found in: ${sessions_dir}"
        return 1
    fi

    local jq_snippet_program
    jq_snippet_program="$(cat <<'EOF'
first(inputs
      | select(.type == "user")
      | (.message.content? // empty)
      | if type == "string" then .
        else (.[]? | objects | select(.type == "text") | .text) end
      | select(test("\\S")))
EOF
)" @TRET

    local -a lines
    local f mtime rel snippet
    for f in "${session_files[@]}" ; do
        mtime="$(zstat -F '%Y-%m-%d %H:%M' +mtime "$f")" @TRET
        rel="${f#"${sessions_dir}/"}"
        snippet="$(jq --raw-output --null-input "${jq_snippet_program}" "$f" 2>/dev/null)" || true
        snippet="${${snippet//$'\n'/ }[1,120]}"
        lines+=("${f}"$'\t'"${mtime}"$'\t'"${rel}"$'\t'"${snippet}")
    done

    local selected
    selected="$(ec "${(F)lines}" | fz --delimiter=$'\t' --with-nth='2..' --no-multi "${fz_opts[@]}")" @RET
    selected="${selected%%$'\n'*}"

    local session_file="${selected%%$'\t'*}"
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
