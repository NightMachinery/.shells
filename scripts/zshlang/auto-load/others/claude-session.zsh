##
#: Claude Code's side of the agent session helpers: the adapter verbs
#: =agent-session.zsh= dispatches to ([agfi:h-agent-session-call]), the
#: profile-aware resume and import that only Claude has, and the old
#: `claude-code-*' names, kept as aliases of the `agent-*' ones. Rendering and
#: scanning live in =golang/agent_session=; see its =readme.org= and
#: =docs/agent-sessions.md=.
##
aliasfn h-claude-code-session-dep h-agent-session-dep
#: The old binary name, for anything outside this file still calling it.
aliasfn claude_session agent_session claude

function claude-code-session-current-id {
    #: The id of the Claude Code session that spawned this shell. Claude Code
    #: exports it into every shell it runs, `! cmd' at its prompt included; a
    #: plain shell has none, and that is the one failure here.
    ##
    local id="${CLAUDE_CODE_SESSION_ID}"
    if test -z "${id}" ; then
        ecerr "$0: not inside a Claude Code session (CLAUDE_CODE_SESSION_ID is unset)"
        return 1
    fi

    ec "${id}"
}
aliasfn sesid claude-code-session-current-id

function h-claude-session-current-id {
    #: The adapter verb ([agfi:h-agent-session-call]), so agent-neutral code
    #: can ask any of the three which session it is in without a `case'. Codex
    #: and agy have had one since they were added; Claude Code's answer was
    #: only reachable under its own name.
    ##
    claude-code-session-current-id
}

function claude-code-session-current-file {
    #: The transcript of the Claude Code session that spawned this shell,
    #: located from the environment Claude Code exports into every shell it
    #: runs. Works from a `! cmd' typed at the Claude Code prompt, too.
    ##
    #: Claude Code's own shell runs commands under NO_BARE_GLOB_QUAL, which
    #: turns `(N)' into a literal and this glob into a "no matches" error.
    setopt localoptions bareglobqual

    local id
    id="$(claude-code-session-current-id)" @RET

    #: Every profile's projects directory, not just this shell's
    #: CLAUDE_CONFIG_DIR: session ids are unique, so searching them all costs
    #: nothing and does not trust the environment further than it must. The
    #: project directory encodes the launch cwd; globbing beats re-encoding it.
    local -a dirs files
    dirs=( ${(f)"$(h-claude-code-session-projects-dirs)"} ) @TRET

    local d
    for d in "${dirs[@]}" ; do
        files+=( "${d}"/*/"${id}".jsonl(N) )
    done

    if (( ${#files} == 0 )) ; then
        ecerr "$0: no transcript for session ${id} under: ${(j:, :)dirs}"
        return 1
    fi

    ec "${files[1]}"
}

function claude-code-session-current-name {
    #: The name of the Claude Code session that spawned this shell; see
    #: [agfi:h-claude-code-session-name] for which of its names wins.
    ##
    local file
    file="$(claude-code-session-current-file)" @RET

    h-claude-code-session-name "${file}"
}


function h-claude-code-session-projects-dirs {
    #: Every Claude Code profile's projects directory, one per line.
    #:
    #: Claude Code keeps its state under `$CLAUDE_CONFIG_DIR`, and
    #: [agfi:claude-work] runs a second config home for the work account, so
    #: there is more than one of these and a session started there is
    #: otherwise invisible to the picker. A glob rather than a written-out
    #: list, so a third profile needs no wiring -- the same reasoning as the
    #: socket glob in [agfi:h-claude-code-session-kitty-socket].
    #:
    #: Claude Code's own shell runs commands under NO_BARE_GLOB_QUAL, so the
    #: `(N/)' below needs the option back to be a qualifier at all.
    ##
    setopt localoptions bareglobqual

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


function h-claude-code-hook-transcript {
    #: The `transcript_path' of the hook payload, or nothing. Fails only when
    #: there was no payload at all.
    ##
    local input
    input="$(h-agent-hook-payload "${1}")"
    test -n "$input" || return 1

    ec "$input" | jq -r '.transcript_path // empty' 2>/dev/null
}

function claude-code-profile-of-transcript {
    #: Which profile owns a transcript, from the projects directory it sits in.
    #: The hook-side twin of [agfi:claude-code-profile-current], which reads
    #: the environment instead; a hook body runs in the garden and has no
    #: CLAUDE_CONFIG_DIR to read.
    ##
    local transcript="${1}"
    assert-args transcript @RET

    local p dir
    for p in "${claude_code_profile_order[@]}" ; do
        dir="${claude_code_profiles[$p]:-${HOME}/.claude}"
        if [[ "${transcript}" == "${dir%/}"/projects/* ]] ; then
            ec "${p}"
            return 0
        fi
    done

    #: <config dir>/projects/<project>/<id>.jsonl, unregistered: name the dir.
    local home="${transcript:h:h:h}"
    ec "${${home:t}#.}"
}

function h-claude-session-account {
    #: Which profile and account a transcript belongs to: `profile work ·
    #: someone@example.com'. The profile comes from the projects directory the
    #: transcript sits in, and the email from that config home's
    #: `.claude.json', which is where Claude Code records the signed-in
    #: account. Two profiles are two accounts, and which one a transcript came
    #: from is otherwise invisible in a rendered document.
    ##
    local transcript="${1}"
    assert-args transcript @RET

    local profile home email
    profile="$(claude-code-profile-of-transcript "${transcript}" 2>/dev/null)" || profile=''

    if test -n "${profile}" ; then
        home="$(h-claude-code-profile-config-home "${profile}" 2>/dev/null)" || home=''
    fi
    : "${home:=${HOME}/.claude}"

    #: The default profile keeps its `.claude.json' beside the config home
    #: rather than inside it; a second config home keeps its own.
    local f
    for f in "${home}/.claude.json" "${HOME}/.claude.json" ; do
        test -e "${f}" || continue
        email="$(jq -r '.oauthAccount.emailAddress // empty' "${f}" 2>/dev/null)" || email=''
        test -n "${email}" && break
    done

    local -a parts
    test -n "${profile}" && parts+=( "profile ${profile}" )
    test -n "${email}" && parts+=( "${email}" )
    (( ${#parts} )) || return 1

    print -r -- "${(j: · :)parts}"
}

function h-claude-code-session-tmux-name {
    : "prints the tmux session name for a Claude Code transcript: '+Claude/<profile> <name>'"
    #: The marker is [agfi:agent_tmux_name_marker]; it says the hooks own this
    #: name and keep it current, as opposed to one a person chose with
    #: [agfi:tmux-session-rename-current]. Shared by
    #: [agfi:tmux-session-rename-current-auto] and the hook, so the two can
    #: never disagree about what a session should be called.
    ##
    local transcript="${1}"
    assert-args transcript @RET

    local profile name
    profile="$(claude-code-profile-of-transcript "${transcript}")" @RET
    name="$(h-claude-code-session-name "${transcript}")" @RET

    #: A space, not a hyphen, between the agent and the name: tmux allows it,
    #: and it reads as two things, which it is.
    ec "${agent_tmux_name_marker}Claude/${profile} ${name}"
}



function claude-code-session-register {
    #: The body of Claude Code's `SessionStart' and `UserPromptSubmit' hooks in
    #: =configFiles/claude-code/settings.json=: [agfi:agent-session-register]
    #: for Claude. `$1' is the hook's pid, which an older ancestry walk needed
    #: and nothing uses now; it stays because the hook line passes it.
    #: Usage: claude-code-session-register <hook-pid> [payload]
    ##
    agent-session-register claude "${2}"
}

function h-claude-session-live-list {
    #: Every live Claude Code session, one per line, tab separated: pid, session
    #: id, name, cwd, transcript, tmux session (or `-'), status. The `live-list'
    #: adapter verb; [agfi:h-agent-session-live-list] gathers every agent's.
    #:
    #: The work is done by the `live' subcommand of the `agent_session' Go
    #: binary: it reads Claude Code's own session records,
    #: =<config home>/sessions/<pid>.json= -- one file per running session,
    #: carrying the pid, session id, name, cwd, busy/idle status and the tmux
    #: location it launched in -- drops any whose pid is no longer alive, and
    #: derives the transcript path. That is exactly what `claude agents --json'
    #: prints, minus finished background agents with no pid, and it costs file
    #: reads instead of ~170ms of subprocess per config home. In shell those
    #: calls were serial and the resolver spent most of half a second here; see
    #: golang/agent_session/internal/claude/live.go.
    #:
    #: [agfi:h-claude-code-session-live-list-sh] is the shell fallback for a
    #: host where the binary is not built. It still shells out to
    #: `claude agents --json', which is slower but prints the same columns.
    ##
    local -a projects_dirs
    projects_dirs=("${(@f)$(h-claude-code-session-projects-dirs)}") @TRET

    #: The hot path must not pay for a build check every time, so probe with
    #: `command -v' (a real PATH lookup) rather than `isdefined-cmd', whose
    #: answer comes from zsh's command hash -- and the garden's hash is stale
    #: for a binary installed after it started, which would send every call
    #: down the build path. `h-agent-session-dep' (which also probes for `go'
    #: and can rebuild) runs only on a genuine first miss.
    if ! command -v agent_session > /dev/null 2>&1 ; then
        h-agent-session-dep 2>/dev/null || true
    fi

    if command -v agent_session > /dev/null 2>&1 ; then
        agent_session claude live "${projects_dirs[@]}" && return 0
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
    #: `agent_session claude name' prefers, and a `custom-title' line, which is what
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
    name="$(agent_session claude name "${source}")" @RET
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
    #: Anything after the second argument goes to the launcher, and the
    #: session resumes in the directory it was working in rather than in this
    #: one ([agfi:h-agent-session-resume-run]; `agent_session_resume_cd_p=n'
    #: for the old behaviour, which warned instead of moving).
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

    #: In the session's own directory, which this used to only warn about; see
    #: [agfi:h-agent-session-resume-run] and `agent_session_resume_cd_p'.
    h-agent-session-resume-run "${transcript}" \
        "${launcher}" --resume "${transcript:t:r}" "${extra[@]}"
}
aliasfn claude-resume claude-code-session-resume

function claude-code-session-resume-fz {
    #: Picks a session with [agfi:h-claude-code-session-select-fz] -- every
    #: profile's copy of the current project, rows labelled by profile -- and
    #: hands it to [agfi:claude-code-session-resume].
    #:
    #: Usage: claude-code-session-resume-fz [to-profile] [claude args...]
    ##
    #: `project' (default), or `all' to choose from every project's sessions
    #: rather than this directory's. The picker's own knob under a name of our
    #: own, so the `-all-fz' variants below can set it the way the viewers do.
    local scope="${claude_code_session_resume_scope:-project}"
    local claude_code_view_session_fz_scope="${scope}"

    local source
    source="$(h-claude-code-session-select-fz)" @RET

    claude-code-session-resume "${source}" "$@"
}
aliasfn claude-resume-fz claude-code-session-resume-fz
#: Same, but selects from the sessions of all projects.
aliasfn claude-code-session-resume-all-fz claude_code_session_resume_scope=all claude-code-session-resume-fz
aliasfn claude-resume-all-fz claude-code-session-resume-all-fz

function claude-resume-personal {
    #: [agfi:claude-code-session-resume] into the default profile: continue a
    #: work session on the personal account.
    #: Usage: claude-resume-personal <transcript|uuid> [claude args...]
    ##
    claude-code-session-resume "${1}" default "${@[2,-1]}"
}
aliasfn claude-resume-personal-fz claude-code-session-resume-fz default
aliasfn claude-resume-personal-all-fz claude_code_session_resume_scope=all claude-code-session-resume-fz default

function claude-resume-work {
    #: [agfi:claude-code-session-resume] into the work profile.
    #: Usage: claude-resume-work <transcript|uuid> [claude args...]
    ##
    claude-code-session-resume "${1}" work "${@[2,-1]}"
}
aliasfn claude-resume-work-fz claude-code-session-resume-fz work
aliasfn claude-resume-work-all-fz claude_code_session_resume_scope=all claude-code-session-resume-fz work
##
#: The adapter: what =agent-session.zsh= asks of an agent, spelled
#: `h-claude-session-<verb>'. See =docs/agent-sessions.md=.
##
aliasfn h-claude-session-roots h-claude-code-session-projects-dirs
aliasfn h-claude-session-resolve h-claude-code-session-resolve
aliasfn h-claude-session-current-id claude-code-session-current-id
aliasfn h-claude-session-hook-transcript h-claude-code-hook-transcript
aliasfn h-claude-session-tmux-name h-claude-code-session-tmux-name

function h-claude-session-owns-p {
    #: Whether a transcript is Claude Code's: it sits under one of the projects
    #: directories. A test on the path alone, so it costs nothing per row.
    ##
    local transcript="${1:a}"

    local d
    for d in ${(f)"$(h-claude-code-session-projects-dirs 2>/dev/null)"} ; do
        [[ "${transcript}" == "${d:a}"/* ]] && return 0
    done
    return 1
}

function h-claude-session-id-of {
    #: `<projects>/<encoded cwd>/<uuid>.jsonl' -> the uuid.
    ##
    ec "${1:t:r}"
}

function h-claude-session-resume {
    #: [agfi:claude-code-session-resume] in the session's own profile.
    #: Usage: h-claude-session-resume <transcript> [claude args...]
    ##
    local transcript="${1}"
    shift
    claude-code-session-resume "${transcript}" '' "$@"
}
##
#: The names these had before the helpers served every agent. Kept as aliases:
#: the kitty binding, the hooks, =claude.zsh= and muscle memory use them.
#: Where the old name meant "Claude only", `agent_session_agents=claude' says so.
##
aliasfn h-claude-code-session-name h-agent-session-name
aliasfn h-claude-code-session-render h-agent-session-render
aliasfn h-claude-code-session-to-md h-agent-session-to-md
aliasfn h-claude-code-session-to-org h-agent-session-to-org
aliasfn h-claude-code-session-to-org-pandoc h-agent-session-to-org-pandoc
aliasfn h-claude-code-session-to-org-native h-agent-session-to-org-native
aliasfn h-claude-code-session-select-fz agent_session_agents=claude h-agent-session-select-fz
aliasfn h-claude-code-session-live-list agent_session_agents=claude h-agent-session-live-list
aliasfn h-claude-code-session-kitty-socket h-agent-session-kitty-socket
aliasfn h-claude-code-session-of-kitty-window h-agent-session-of-kitty-window
aliasfn h-claude-code-session-preview h-agent-session-preview
aliasfn claude-code-view-session agent-view-session
aliasfn claude-code-view-session-fz agent_session_agents=claude agent-view-session-fz
aliasfn claude-code-view-session-all-fz agent_session_agents=claude agent_session_fz_scope=all agent-view-session-fz
aliasfn claude-code-view-session-md-fz agent_session_agents=claude agent-view-session-md-fz
aliasfn claude-code-view-session-md-all-fz agent_session_agents=claude agent_session_fz_scope=all agent-view-session-md-fz
aliasfn claude-code-view-session-raw-fz agent_session_agents=claude agent-view-session-raw-fz
aliasfn claude-code-view-session-raw-all-fz agent_session_agents=claude agent_session_fz_scope=all agent-view-session-raw-fz
aliasfn claude-code-view-session-focused agent-view-session-focused
aliasfn claude-code-view-session-bg agent-view-session-bg
aliasfn claude-code-view-session-toggle agent-view-session-toggle
aliasfn claude-code-view-sessions agent-view-sessions
aliasfn claude-code-view-reap agent-view-reap
aliasfn claude-code-session-live-fz agent_session_agents=claude agent-session-live-fz
aliasfn claude-session-selftest agent-session-selftest
##
