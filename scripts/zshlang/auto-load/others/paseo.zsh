##
#: Native session handoff to Paseo. See =docs/paseo.md=.
##
function paseo-install {
    : "usage: paseo-install [version]"
    local package='@getpaseo/cli'
    (( $# <= 1 )) || { ecerr "$0: expected at most one version"; return 64; }
    [[ -n "${1:-}" ]] && package+="@${1}"
    #: One flag per package: pnpm does not split a comma list, and records
    #: \`esbuild,msgpackr-extract,node-pty' as a single package name.
    local -a npm_install_pnpm_opts=(--allow-build=esbuild --allow-build=msgpackr-extract --allow-build=node-pty)
    npm-install "${package}" @RET
    h-npm-install-report paseo
    #: An upgrade is when Paseo could change the records paseo-ls reads.
    #: The check needs a running daemon, so it is skipped while the daemon is stopped.
    if command paseo daemon status 2>/dev/null | command grep -q '^localDaemon: running' ; then
        paseo-ls-check >/dev/null || ecerr "$0: paseo-ls no longer matches \`paseo ls'; run paseo-ls-check"
    else
        ecerr "$0: daemon stopped; run paseo-ls-check after \`paseo daemon start'"
    fi
}

function paseo-cli-update {
    : "usage: paseo-cli-update [version]"
    : "Install the latest (or given) Paseo CLI. It neither starts nor restarts the daemon."
    #: Our changes to Paseo (no worktree-setup callout, no push when a
    #: scheduled run finishes) live in the YPaseo fork now, not in patches to
    #: this install; see docs/paseo.md.
    paseo-install "$@"
    local ret=$?
    if [[ "${commands[paseo]:A}" == */ypaseo-daemon/* ]] ; then
        ecerr "$0: note: paseo runs the YPaseo daemon build, which shadows this install; see ~/.ypaseo/daemon-activate.zsh --off"
    fi
    return ${ret}
}

function h-paseo-daemon-env {
    : "usage: h-paseo-daemon-env cmd... ; run cmd without any agent session's environment"
    #: Agents inherit the daemon's environment, and a provider that sets no
    #: CLAUDE_CONFIG_DIR (the personal one) takes the daemon's. A daemon
    #: started from a work-profile session thus ran personal agents on the work
    #: account (2026-11-03). Drop every CLAUDE* and PASEO_AGENT* variable.
    local -a unset_opts=()
    local name
    for name in ${(k)parameters[(I)CLAUDE*]} ${(k)parameters[(I)PASEO_AGENT*]} ; do
        unset_opts+=(-u "${name}")
    done
    #: Agents get the account's login shell: helper initialization can change SHELL.
    local login_shell="${SHELL}"
    if isDarwin ; then
        login_shell="$(command dscl . -read "$HOME" UserShell | command awk '{print $2}')"
    fi
    SHELL="${login_shell}" command env "${unset_opts[@]}" "$@"
}

function paseo-start {
    : "usage: paseo-start [paseo daemon start options]"
    #: A daemon built from the YPaseo fork is staged by ~/.ypaseo/build-daemon.zsh
    #: and switched to only here, while no daemon runs, so that \`paseo' always
    #: matches the daemon it talks to.
    local activate="${HOME}/.ypaseo/daemon-activate.zsh"
    if [[ -x "${activate}" ]] ; then
        "${activate}" || ecerr "$0: could not activate the staged YPaseo daemon; starting the current one"
        rehash
    fi
    h-paseo-daemon-env paseo daemon start "$@"
}

function paseo-restart {
    : "usage: paseo-restart [paseo daemon start options]"
    : "Stop the daemon and its supervisor, then start both from a clean environment."
    : "It kills every agent turn, including the caller's: from an agent, run it detached,"
    : "e.g. tmux-job-start paseo-restart zsh -ic paseo-restart"
    #: \`paseo daemon restart' keeps the supervisor, whose environment the new worker inherits.
    command paseo daemon stop @RET
    paseo-start "$@"
}

function 2paseo {
    : "usage: 2paseo [--wait-exit] [--no-switch] [--to <claude-config-dir> [--skip-subagents]] [--max-import-mb <n>]
--to moves a Claude session to another account's config directory, e.g. --to ~/.claude.
--skip-subagents leaves the subagent transcripts out of that copy; --max-import-mb raises the size limit (200)."
    local wait_p=n switch_p=y to='' skip_p=n max_mb=''
    while (( $# )) ; do
        case "${1}" in
            --wait-exit) wait_p=y ;;
            --no-switch) switch_p=n ;;
            --skip-subagents) skip_p=y ;;
            --max-import-mb|--max-import-mb=*)
                if [[ "${1}" == --max-import-mb ]] ; then
                    (( $# >= 2 )) || { ecerr "$0: --max-import-mb needs a number of megabytes"; return 64; }
                    max_mb="${2}"
                    shift
                else
                    max_mb="${1#--max-import-mb=}"
                fi
                [[ "${max_mb}" == <1-> ]] || { ecerr "$0: --max-import-mb needs a positive whole number: ${max_mb}"; return 64; }
                ;;
            --to|--to=*)
                if [[ "${1}" == --to ]] ; then
                    (( $# >= 2 )) || { ecerr "$0: --to needs a Claude config directory"; return 64; }
                    to="${2}"
                    shift
                else
                    to="${1#--to=}"
                fi
                #: A quoted `~/' arrives unexpanded. Expand it; refuse any other relative path.
                [[ "${to}" == '~' || "${to}" == '~/'* ]] && to="${HOME}${to#\~}"
                [[ "${to}" == /* ]] || { ecerr "$0: --to needs an absolute or ~/ path: ${to}"; return 64; }
                #: paseo_handoff.py checks that it is another Claude config directory.
                to="${to:A}"
                ;;
            *) ecerr "$0: unsupported option: ${1}"; return 64 ;;
        esac
        shift
    done
    ensure-cmd paseo python3 tmux gmktemp @RET
    h-tmux-env-repair || true
    local paseo_bin="$(whence -p paseo)"
    local paseo_home="${paseo_home:-${PASEO_HOME:-${HOME}/.paseo}}"
    local uuid_re='^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$'
    local name id="${PASEO_AGENT_ID:-}"
    if [[ -n "${id}" ]] ; then
        [[ "${id}" =~ ${uuid_re} ]] || { ecerr "$0: invalid Paseo agent ID"; return 1; }
        [[ -z "${to}${max_mb}" && "${skip_p}" == n ]] || {
            ecerr "$0: already a Paseo agent; --to, --skip-subagents and --max-import-mb apply only to a native session"
            return 1
        }
        name="paseo-${id}"
        if ! tmux-job-running-p "${name}" ; then
            tmux-job-start "${name}" "${paseo_bin}" attach "${id}" --home "${paseo_home}" @RET
        fi
    else
        h-agent-session-dep @RET
        local agent transcript dir provider_home rows row source_pid='' bundle
        local agent_session_live_list_cache='' claude_code_session_live_list_cache=''
        local -a fields opts=()
        agent="$(agent-session-current-agent)" @RET
        [[ "${agent}" == (claude|codex) ]] || { ecerr "$0: only Claude and Codex sessions can be imported"; return 1; }
        [[ -z "${to}" || "${agent}" == claude ]] || { ecerr "$0: --to moves Claude sessions only; a Codex session keeps its CODEX_HOME"; return 1; }
        transcript="$(agent-session-current-file)" @RET
        id="$(h-agent-session-call "${agent}" id-of "${transcript}")" @RET
        [[ "${id}" =~ ${uuid_re} ]] || { ecerr "$0: current session has no exact UUID"; return 1; }
        dir="$(h-agent-session-dir "${transcript}" "${agent}")" @RET
        if [[ "${agent}" == claude ]] ; then
            provider_home="${transcript:a:h:h:h}"
        else
            provider_home="$(h-codex-session-home)" @RET
        fi
        rows="$(h-agent-session-live-list)" @RET
        for row in "${(@f)rows}" ; do
            fields=( "${(@ps:\t:)row}" )
            (( ${#fields} >= 5 )) || continue
            if [[ "${fields[5]:a}" == "${transcript:a}" ]] ; then
                [[ -z "${source_pid}" ]] || { ecerr "$0: multiple processes own this transcript"; return 1; }
                source_pid="${fields[1]}"
            fi
        done
        [[ "${source_pid}" == <-> ]] || { ecerr "$0: cannot identify the current native process"; return 1; }
        name="paseo-${id}"
        local state_root="${paseo_state_dir:-${XDG_STATE_HOME:-${HOME}/.local/state}/agent-handoffs}"
        bundle="$(
            umask 077
            command mkdir -p -- "${state_root}" @RET
            gmktemp --directory "${state_root}/paseo.XXXXXXXX"
        )" @RET
        zmodload zsh/system @RET
        [[ "${wait_p}" == y ]] && opts+=(--wait-exit)
        [[ -n "${to}" ]] && opts+=(--target-home "${to}")
        #: paseo_handoff.py refuses --skip-subagents without --to, and says why.
        [[ "${skip_p}" == y ]] && opts+=(--skip-subagents)
        [[ -n "${max_mb}" ]] && opts+=(--max-import-mb "${max_mb}")
        command python3 "${NIGHTDIR}/python/paseo_handoff.py" prepare \
            --agent "${agent}" --id "${id}" --transcript "${transcript}" \
            --cwd "${dir}" --provider-home "${provider_home}" \
            --source-pid "${source_pid}" --caller-pid "${sysparams[pid]}" \
            --paseo "${paseo_bin}" --agent-session "$(whence -p agent_session)" \
            --paseo-home "${paseo_home:a}" --tmux-session "${name}" \
            --output "${bundle}/plan.json" "${opts[@]}" @RET
        tmux-job-start "${name}" command python3 "${NIGHTDIR}/python/paseo_handoff.py" \
            run "${bundle}/plan.json" @RET
        if [[ "${wait_p}" == y ]] ; then
            ecerr "$0: handoff queued; exit the original session to complete it"
        else
            ecerr "$0: Paseo preflight will run, then gracefully stop the validated native process"
        fi
        [[ -z "${to}" ]] || ecerr "$0: once it has exited, the session is copied to ${to} and imported there"
        [[ "${skip_p}" == n ]] || ecerr "$0: the copy leaves out the subagent transcripts; the originals stay in place"
        ecerr "$0: recovery state: ${bundle}"
    fi
    if [[ "${switch_p}" == y && -n "${TMUX:-}" ]] ; then
        command tmux switch-client -t "=${name}"
    else
        ecerr "$0: output is attached in ${name}; tmux attach-session -t =${name}"
    fi
}
##
#: Paseo agents in the session pickers. See "Native sessions" in =docs/paseo.md=.
##
#: Put ahead of the agent glyph on a Paseo row, the way
#: [agfi:h-agent-session-annotate-rows] places any badge.
typeset -g paseo_agent_badge="${paseo_agent_badge:-🧭}"

function h-paseo-home {
    #: The local daemon home Paseo rows are read from and sends go to. A remote
    #: `PASEO_HOST' is refused, as =paseo_auto_continue.py= refuses it: that
    #: daemon's records are not on this disk, and a job firing hours later
    #: cannot promise to reach it.
    ##
    if [[ -n "${PASEO_HOST:-}" ]] ; then
        ecerr "$0: remote PASEO_HOST is unsupported; run on the daemon host"
        return 1
    fi
    ec "${paseo_home:-${PASEO_HOME:-${HOME}/.paseo}}"
}

function paseo-schedules-export {
    : "usage: paseo-schedules-export [out-dir]
Export this host's Paseo schedule definitions (no run history) for version
control, by default into the private notes repository. See =docs/paseo.md=."
    ##
    local home out
    home="$(h-paseo-home)" @RET
    out="${1:-${nightNotesPrivate}/configs/paseo-config/schedules/$(command hostname -s)}"
    [[ -n "${nightNotesPrivate}" || -n "${1:-}" ]] || { ecerr "$0: nightNotesPrivate is unset; pass an output directory"; return 1; }
    command python3 "${NIGHTDIR}/python/paseo_schedules_export.py" --src "${home}/schedules" --out "${out}"
}

function paseo-ls {
    : "usage: paseo-ls [--json]
The agents \`paseo ls --global' lists, read from the daemon's stored records
without asking the daemon. Tab separated: uuid, native agent, native session
id, status, provider, title, cwd; \`-' where unknown. --json prints objects."
    ##
    #: The one reader of the records, so a Paseo upgrade that changes their
    #: layout is fixed here once; [agfi:paseo-ls-check] says when it has.
    #: The same agents and statuses as the CLI (checked against 0.10.2), in
    #: 20ms instead of 900ms, plus the one field the CLI does not print:
    #: `.persistence.nativeHandle', the native session the agent runs, which
    #: the live listing also reports as a session in its own right; see
    #: [agfi:h-paseo-filter-pairs].
    ##
    setopt localoptions bareglobqual

    local json_p=n
    case "${1:-}" in
        '') ;;
        --json) json_p=y ;;
        *) ecerr "$0: unknown argument: ${1}" ; return 64 ;;
    esac

    ensure-cmd jq @RET

    local home
    home="$(h-paseo-home)" @RET

    local -a records
    records=( "${home}"/agents/*.json(N.) "${home}"/agents/*/*.json(N.) )
    if (( ${#records} == 0 )) ; then
        bool "${json_p}" && ec '[]'
        return 0
    fi

    local agents='
        [ inputs
          | select((.archivedAt // null) == null and (.internal // false) == false)
          | { id,
              nativeAgent: (.persistence.metadata.provider // "-"),
              nativeSessionId: (.persistence.nativeHandle // .runtimeInfo.sessionId // "-"),
              status: (.lastStatus // "-"), provider: (.provider // "-"),
              title: (.title // "-"), cwd: (.cwd // "-"),
              parentAgentId: (.labels["paseo.parent-agent-id"] // null),
              keep: ((.labels.keep // "") | tostring | ascii_downcase == "true"),
              lastActivityAt: (.lastActivityAt // null),
              lastUserMessageAt: (.lastUserMessageAt // null) } ]'

    if bool "${json_p}" ; then
        command jq --null-input "${agents}" "${records[@]}"
    else
        command jq --null-input --raw-output "${agents}"'
            | .[]
            | [ .id, .nativeAgent, .nativeSessionId, .status, .provider, .title, .cwd ]
            | map(tostring | gsub("[\t\n]"; " ")) | join("\t")' "${records[@]}"
    fi
}

function paseo-ls-check {
    : "usage: paseo-ls-check
Compares paseo-ls with the daemon's own \`paseo ls --global'. Fails when they
list different agents, which means Paseo changed its stored records."
    ##
    #: Ids must agree. A status may differ, as an agent can finish between the
    #: two calls, so that is only reported.
    ##
    ensure-cmd paseo jq @RET

    local home ours theirs
    home="$(h-paseo-home)" @RET
    ours="$(paseo-ls --json)" @RET
    theirs="$(command paseo ls --global --json --home "${home}")" @TRET

    local report
    report="$(command jq --null-input --raw-output --argjson ours "${ours}" --argjson theirs "${theirs}" '
        ($ours | map({key: .id, value: .status}) | from_entries) as $o
        | ($theirs | map({key: .id, value: .status}) | from_entries) as $t
        | ([$t | keys[] | select($o[.] == null) | "missing from paseo-ls: \(.)"]
           + [$o | keys[] | select($t[.] == null) | "extra in paseo-ls: \(.)"]) as $bad
        | ([$o | keys[] | select($t[.] != null and $t[.] != $o[.]) | "status \($o[.]) here, \($t[.]) per the daemon: \(.)"]) as $note
        | ($bad | map("error: " + .)) + ($note | map("note: " + .))
          + ["\($bad | length) mismatched of \($t | length) agents"]
        | .[]')" @TRET

    ec "${report}"
    [[ "${report}" == *$'\n'"0 mismatched of "* || "${report}" == "0 mismatched of "* ]]
}

function h-paseo-filter-pairs {
    #: Drops, from `<caller><TAB><transcript>...' rows on stdin, the native
    #: sessions a Paseo agent runs, so a picker that also offers the Paseo
    #: agent lists it once. The native row is the wrong one to keep: it is a
    #: headless `--input-format stream-json' process under the Paseo daemon,
    #: so no window or pane holds it and only Paseo can talk to it, yet the
    #: live listing still gives it a tmux session name of its own.
    #:
    #: Matched by transcript, through the live listing's id column. Passes
    #: everything through when there is no Paseo to ask, so a missing daemon
    #: home never empties a picker.
    ##
    local paseo_rows
    paseo_rows="$(paseo-ls 2>/dev/null)" || paseo_rows=''
    if test -z "${paseo_rows}" ; then
        command cat
        return 0
    fi

    local row
    local -a f
    local -A native hide
    for row in ${(f)paseo_rows} ; do
        f=( "${(@ps:\t:)row}" )
        [[ "${f[3]}" != '-' ]] && native[${f[3]}]=1
    done
    for row in ${(f)"$(h-agent-session-live-list 2>/dev/null)"} ; do
        f=( "${(@ps:\t:)row}" )
        test -n "${f[2]}" && test -n "${f[5]}" || continue
        (( ${+native[${f[2]}]} )) && hide[${f[5]}]=1
    done

    while IFS= read -r row ; do
        test -n "${row}" || continue
        f=( "${(@ps:\t:)row}" )
        (( ${+hide[${f[2]}]} )) && continue
        ec "${row}"
    done
}

function h-paseo-agent-pairs {
    #: The Paseo agents as rows for [agfi:h-agent-session-annotate-rows]:
    #: `<prefix><uuid><TAB><transcript><TAB><agent><TAB><label><TAB><badge>
    #: <TAB><fallback epoch><TAB><fallback stamp>'.
    #: The transcript is the native session's, so the pickers' preview and
    #: alt+enter work on these rows unchanged; `-' when none is on disk.
    #:
    #: A glob per row rather than `h-agent-session-call <agent> resolve': that
    #: costs 100ms each, which twenty agents turn into two seconds before the
    #: picker opens.
    #: Usage: h-paseo-agent-pairs <caller-prefix>
    ##
    setopt localoptions bareglobqual

    local subagents_p="${agent_session_fz_subagents_p:-n}"
    local sort_by="${agent_session_rows_sort-last}"
    #: Newline-separated agent UUIDs: only these, subagents or not. Applied
    #: before the per-row work below, which costs about 0.2s an agent.
    local only_ids="${paseo_agent_pairs_ids:-}"
    local prefix="${1}"
    bool "${subagents_p}" && subagents_p=y

    local paseo_rows
    #: Keep paseo-ls complete: the duplicate filter needs to hide children's
    #: native processes too. Only the picker applies this parent filter.
    paseo_rows="$(paseo-ls --json | command jq --raw-output \
        --arg subagents "${subagents_p}" --arg sort_by "${sort_by}" --arg only "${only_ids}" '
        ($only | split("\n") | map(select(. != "")) | map({(.): true}) | add // null) as $only_set
        | .[]
        | select(if $only_set then $only_set[.id] == true
                 else ($subagents == "y" or (.parentAgentId // "") == "") end)
        | (if $sort_by == "user" then (.lastUserMessageAt // .lastActivityAt)
           else .lastActivityAt end) as $stamp
        | (try ($stamp | sub("\\.[0-9]+Z$"; "Z") | fromdateiso8601) catch 0) as $epoch
        | [ .id, .nativeAgent, .nativeSessionId, .status, .provider, .title, .cwd,
            $epoch, ($stamp // "?") ]
        | map(tostring | gsub("[\t\n]"; " ")) | join("\t")')" @RET
    test -n "${paseo_rows}" || return 0

    local -A live_t
    local row
    local -a f
    for row in ${(f)"$(h-agent-session-live-list 2>/dev/null)"} ; do
        f=( "${(@ps:\t:)row}" )
        test -n "${f[2]}" && test -n "${f[5]}" && live_t[${f[2]}]="${f[5]}"
    done

    local -a claude_roots codex_roots hits
    claude_roots=( ${(f)"$(h-agent-session-call claude roots 2>/dev/null)"} )
    codex_roots=( ${(f)"$(h-agent-session-call codex roots 2>/dev/null)"} )

    local t r
    for row in ${(f)paseo_rows} ; do
        f=( "${(@ps:\t:)row}" )
        t="${live_t[${f[3]}]:-}"
        if test -z "${t}" && [[ "${f[3]}" != '-' ]] ; then
            hits=()
            case "${f[2]}" in
                claude)
                    for r in "${claude_roots[@]}" ; do
                        hits+=( "${r}"/*/"${f[3]}".jsonl(N.) )
                    done
                    ;;
                codex)
                    for r in "${codex_roots[@]}" ; do
                        hits+=( "${r}"/*/*/*/rollout-*-"${f[3]}".jsonl(N.) )
                    done
                    ;;
            esac
            t="${hits[1]:-}"
        fi

        printf '%s%s\t%s\t%s\t%s  %s (%s)\t%s\t%s\t%s\n' "${prefix}" "${f[1]}" "${t:--}" \
            "${f[2]}" "paseo:${f[1][1,8]}" "${f[6]}" "${f[4]}" "${paseo_agent_badge}" "${f[8]}" "${f[9]}"
    done
}

function h-paseo-archive-rows {
    #: Full UUID, short UUID, status, LAST ACTIVE, provider/model, title, cwd,
    #: protection and cascade description. The planner reads only local records.
    emulate -L zsh
    local scope="${1}" kind="${2}" minimum="${3:-72h}" force="${4:-n}"
    local show_hidden="${5:-n}" summary_p="${6:-n}"
    local caller="${PASEO_AGENT_ID:-}" parent="${PASEO_PARENT_AGENT_ID:-}"
    local root="${NIGHTDIR:-${HOME}/scripts}" archive_home data
    ensure-cmd paseo python3 @RET
    archive_home="$(h-paseo-home)" @RET
    #: Always collect globally: a child in another directory still cascades.
    data="$(command paseo ls --global --all --json)" @RET
    #: --all includes archived rows; the planner filters stored archivedAt.
    local -a opts=()
    [[ "${force}" == y ]] && opts+=(--force)
    [[ "${show_hidden}" == y ]] && opts+=(--show-hidden)
    [[ "${summary_p}" == y ]] && opts+=(--summary)
    print -r -- "${data}" | command python3 "${root}/python/paseo_archive.py" \
        --home "${archive_home}" --scope "${scope}" --kind "${kind}" --cwd "${PWD:A}" \
        --caller "${caller}" --parent "${parent}" --min-idle "${minimum}" "${opts[@]}"
}

function h-paseo-archive-preview-rows {
    #: Append native transcript/provider as hidden columns 12/13. Preserve the
    #: planner's UUID, eligibility, cascade and ordering columns verbatim.
    local pairs row id transcript agent
    local -a fields
    local -A transcript_of agent_of
    pairs="$(agent_session_fz_subagents_p=y h-paseo-agent-pairs '' 2>/dev/null)" || pairs=''
    for row in "${(@f)pairs}" ; do
        fields=( "${(@ps:\t:)row}" )
        id="${fields[1]}"; transcript="${fields[2]}"; agent="${fields[3]}"
        transcript_of[${id}]="${transcript:--}"
        case "${agent}" in
            claude|codex|agy) agent_of[${id}]="${agent}" ;;
        esac
    done
    while IFS= read -r row ; do
        test -n "${row}" || continue
        id="${row%%$'\t'*}"
        printf '%s\t%s\t%s\n' "${row}" "${transcript_of[$id]:--}" "${agent_of[$id]:--}"
    done
}

function h-paseo-archive {
    emulate -L zsh
    setopt pipefail
    local kind="${1}" ; shift
    local scope=global dry=n yes=n force=n filter='' filter_p=n minimum=72h show_hidden=n summary='' eligible_rows=''
    local row rows selected id failed=0 answer
    local -a fields archived_ids=()
    local -A allowed covered
    while (( $# )) ; do
        case "${1}" in
            -l|--local-dir) scope=local ;;
            -g|--global) scope=global ;;
            -n|--dry-run) dry=y ;;
            -y|--yes) yes=y ;;
            --show-hidden) show_hidden=y ;;
            --all-ages) minimum=0 ;;
            --min-idle)
                (( $# >= 2 )) || { ecerr "$0: --min-idle needs 72h, 3d, 90m, or 0"; return 64; }
                minimum="${2}" ; shift ;;
            --force)
                [[ "${kind}" == fz ]] || { ecerr "$0: --force is only available in the picker"; return 64; }
                force=y ;;
            --filter)
                (( $# >= 2 )) || { ecerr "$0: --filter needs text"; return 64; }
                filter="${2}" filter_p=y ; shift ;;
            -h|--help)
                print -r -- 'usage: paseo-archive-fz [-l] [-n] [--min-idle DUR|--all-ages] [--show-hidden] [--force] [--filter TEXT]
       paseo-archive-all [-l] [-n] [-y] [--min-idle DUR|--all-ages] [--show-hidden]
Default: idle or terminal agents last active at least 72 hours ago, oldest first,
across every directory. --local-dir (-l) restricts cwd; --global (-g) is default.
An agent whose directory was deleted (missing, with its parent present) is
offered at any age, marked "(folder gone)"; once its workspace has no agents
left, the workspace is archived too, since nothing else removes it.
--min-idle accepts 72h, 3d, 90m, or 0; --all-ages is --min-idle 0.
Terminal statuses: closed, error, errored, failed, stopped; archived rows are excluded.
LAST ACTIVE uses stored lastActivityAt, then updatedAt, then createdAt.
Archiving a parent also archives its subagents. Parents with ineligible
children are hidden. Each row/confirmation lists the affected descendants.
Picker --force overrides age and a selected agent running itself, but NEVER
allows a parent with a running descendant. Pinned agents, the caller and all
its ancestors cannot be archived, including through cascades. --show-hidden
lists excluded rows with REASON separately; they never enter the picker.
The footer counts hidden agents and reasons. Local-dir scope also cannot be
bypassed, even through a cascade. Archive-all never offers running agents.
Missing local records block planning; remote daemon targets are unsupported.
Archive-all confirms on a terminal; otherwise --yes is required. --dry-run
never archives. --filter uses non-interactive fzf selection.
Archive is soft deletion; this CLI has no unarchive command. App recovery
support depends on the installed version. paseo-keep ID pins; paseo-unkeep ID
clears the pin by setting keep=false (the CLI has no label-delete option).'
                return 0 ;;
            *) ecerr "$0: unknown option: ${1}"; return 64 ;;
        esac
        shift
    done
    [[ "${kind}" == fz || "${filter_p}" == n ]] || { ecerr "$0: --filter is only for the picker"; return 64; }
    rows="$(h-paseo-archive-rows "${scope}" "${kind}" "${minimum}" "${force}" "${show_hidden}" y)" @RET
    for row in "${(@f)rows}" ; do
        if [[ "${row}" == '#summary'$'\t'* ]] ; then
            summary="${row#*$'\t'}"
            continue
        fi
        fields=( "${(@ps:\t:)row}" )
        if [[ "${fields[8]}" == hidden ]] ; then
            print -r -- "HIDDEN ${fields[2]}  ${fields[6]}  LAST ACTIVE ${fields[4]}  REASON: ${fields[11]}"
        else
            eligible_rows+="${row}"$'\n'
        fi
    done
    rows="${eligible_rows%$'\n'}"
    if [[ -z "${rows}" ]] ; then
        print -r -- "${summary}"
        if [[ "${scope}" == local ]] ; then
            print -r -- 'No eligible agents in this directory; drop --local-dir to see every directory.'
        else
            print -r -- 'No eligible agents.'
        fi
        return 0
    fi
    for row in "${(@f)rows}" ; do
        fields=( "${(@ps:\t:)row}" )
        allowed[${fields[1]}]="${fields[8]}"
    done
    if [[ "${kind}" == fz ]] ; then
        ensure-cmd fzf @RET
        rows="$(print -r -- "${rows}" | h-paseo-archive-preview-rows)" @RET
        local rich_preview preview
        rich_preview="$(h-agent-session-preview-cmd '{13}')" @RET
        preview="printf '%s\\n\\n' {9}; ${rich_preview} {12}"
        local -a fz_args=(--multi --ansi --delimiter=$'\t' --with-nth=2..7,9 \
            --header="ID | STATUS | LAST ACTIVE | PROVIDER | TITLE | CWD | CASCADE; default inactive >=72h
${summary}" \
            --preview "${preview}" --preview-window=down,60%,wrap)
        [[ "${filter_p}" == y ]] && fz_args+=(--filter "${filter}")
        selected="$(print -r -- "${rows}" | fzf-gateway "${fz_args[@]}")" || {
            local pick_rc=$?
            print -r -- "${summary}"
            return "${pick_rc}"
        }
    else
        selected="${rows}"
    fi
    if [[ -z "${selected}" ]] ; then
        print -r -- "${summary}"
        return 0
    fi
    print -r -- $'ID\tSTATUS\tLAST ACTIVE\tPROVIDER/MODEL\tTITLE\tCWD'
    for row in "${(@f)selected}" ; do
        fields=( "${(@ps:\t:)row}" )
        print -r -- "${(pj:\t:)fields[2,7]}"
        [[ "${fields[9]}" == '-' ]] || print -r -- "    ${fields[9]}"
    done
    print -r -- "${summary}"
    [[ "${dry}" == y ]] && return 0
    if [[ "${kind}" == all && "${yes}" != y ]] ; then
        if [[ ! -t 0 ]] ; then
            ecerr 'paseo-archive-all: stdin is not a terminal; --yes is required (or use --dry-run)'
            return 64
        fi
        print -rn -- 'Archive these agents? [y/N] ' >&2
        IFS= read -r answer || return 1
        [[ "${answer:l}" == y || "${answer:l}" == yes ]] || return 1
    fi
    #: Fresh eligibility catches a changed caller/parent relationship or scope.
    rows="$(h-paseo-archive-rows "${scope}" "${kind}" "${minimum}" "${force}")" @RET
    allowed=()
    for row in "${(@f)rows}" ; do
        fields=( "${(@ps:\t:)row}" )
        allowed[${fields[1]}]="${fields[8]}"
    done
    #: If a selected parent covers another selected row, issue only the
    #: parent's archive request. The daemon owns the asynchronous cascade.
    local child
    for row in "${(@f)selected}" ; do
        fields=( "${(@ps:\t:)row}" )
        for child in "${(@s:,:)fields[10]}" ; do
            [[ "${child}" == '-' ]] || covered[${child}]=y
        done
    done
    local -a archive_opts=()
    [[ "${force}" == y ]] && archive_opts+=(--force)
    #: Declared once, outside the loop: zsh prints an existing variable when
    #: `local` names it again without a value.
    local fresh fresh_row old_cascade old_descendants
    for row in "${(@f)selected}" ; do
        fields=( "${(@ps:\t:)row}" )
        id="${fields[1]}"
        [[ "${covered[$id]:-}" == y ]] && continue
        fresh_row='' old_cascade="${fields[9]}" old_descendants="${fields[10]}"
        fresh="$(h-paseo-archive-rows "${scope}" "${kind}" "${minimum}" "${force}")" @RET
        for fresh_row in "${(@f)fresh}" ; do
            [[ "${fresh_row%%$'\t'*}" == "${id}" ]] && break
            fresh_row=''
        done
        fields=( "${(@ps:\t:)fresh_row}" )
        if [[ -z "${fresh_row}" || "${fields[9]}" != "${old_cascade}" || "${fields[10]}" != "${old_descendants}" ]] ; then
            ecerr "skip ${id}: eligibility or cascade changed; review a fresh listing"
            failed=1
            continue
        fi
        if [[ "${allowed[$id]:-}" != selectable ]] ; then
            ecerr "skip ${id}: protected or no longer eligible"
            failed=1
            continue
        fi
        #: No --force in archive-all: the CLI also guards a run starting after
        #: the fresh listing. Each command prints its own success or failure.
        if command paseo archive "${id}" "${archive_opts[@]}" ; then
            archived_ids+=("${id}" "${(@s:,:)old_descendants:#-}")
        else
            failed=1
        fi
    done
    h-paseo-archive-stale-workspaces "${archived_ids[@]}" || failed=1
    return "${failed}"
}

function h-paseo-archive-stale-workspaces {
    : "usage: h-paseo-archive-stale-workspaces AGENT_ID..."
    #: Archiving an agent leaves its workspace in the sidebar. When the
    #: workspace's directory was deleted (a retired worktree) and none of its
    #: agents remains, nothing else will ever remove it, so archive it too.
    #: Paseo deletes only directories it owns, and this one is already gone.
    emulate -L zsh
    setopt pipefail
    (( $# )) || return 0
    local root="${NIGHTDIR:-${HOME}/scripts}" archive_home stale row failed=0
    local -a fields
    archive_home="$(h-paseo-home)" @RET
    stale="$(command paseo workspace ls --json |
        command python3 "${root}/python/paseo_archive.py" --home "${archive_home}" \
            --stale-workspaces "${(j:,:)@}")" @RET
    for row in "${(@f)stale}" ; do
        [[ -n "${row}" ]] || continue
        fields=( "${(@ps:\t:)row}" )
        print -r -- "workspace ${fields[2]}: folder gone (${fields[3]}); archiving it"
        command paseo workspace archive "${fields[1]}" || failed=1
    done
    return "${failed}"
}

function paseo-archive-fz {
    : "usage: paseo-archive-fz [-l] [-n] [--min-idle DUR|--all-ages] [--show-hidden] [--force] [--filter TEXT]
Pick agents to soft-delete. See --help for running-agent protection and recovery."
    h-paseo-archive fz "$@"
}

function paseo-archive-all {
    : "usage: paseo-archive-all [-l] [-n] [-y] [--min-idle DUR|--all-ages] [--show-hidden]
Soft-delete inactive agents in every directory (-l: only the current one),
protecting the caller and all its ancestors."
    h-paseo-archive all "$@"
}


function paseo-keep {
    : "usage: paseo-keep ID
Set keep=true, protecting this agent and ancestors from archive cascades."
    (( $# == 1 )) && [[ -n "${1}" && "${1}" != -* ]] || { ecerr "$0: one agent ID is required"; return 64; }
    ensure-cmd paseo @RET
    command paseo agent update "${1}" --label keep=true
}

function paseo-unkeep {
    : "usage: paseo-unkeep ID
Clear the pin with keep=false; the CLI has no label-delete option."
    (( $# == 1 )) && [[ -n "${1}" && "${1}" != -* ]] || { ecerr "$0: one agent ID is required"; return 64; }
    ensure-cmd paseo @RET
    command paseo agent update "${1}" --label keep=false
}

##
#: From a Paseo agent id to the native transcript it runs. See
#: =docs/paseo-transcript.md=.
##
function h-paseo-agent-record {
    : "usage: h-paseo-agent-record <id or unique prefix>
The stored record of one agent, archived ones included."
    setopt localoptions bareglobqual

    local id="${(L)1}"
    if [[ ! "${id}" =~ '^[0-9a-f][0-9a-f-]*$' ]] ; then
        ecerr "$0: not a Paseo agent id: ${1}"
        return 64
    fi
    local home
    home="$(h-paseo-home)" @RET

    local -a hits
    hits=( "${home}"/agents/"${id}"*.json(N.) "${home}"/agents/*/"${id}"*.json(N.) )
    if (( ${#hits} == 0 )) ; then
        ecerr "$0: no Paseo agent matches: ${id}"
        return 1
    elif (( ${#hits} > 1 )) ; then
        ecerr "$0: ${#hits} Paseo agents match ${id}: ${(j:, :)${hits[@]:t:r}}"
        return 1
    fi
    ec "${hits[1]}"
}

function h-paseo-native-rows {
    : "usage: h-paseo-native-rows <record>...
One tab separated row per record: id, native agent (claude, codex or -), native
home (empty for the default), session ids (comma separated), cwd, provider,
status, archivedAt, title, keep (true/false), parent id."
    #: The provider is followed through the config's `extends' chain to claude
    #: or codex, as Paseo does, and the first CLAUDE_CONFIG_DIR or CODEX_HOME
    #: met on the way is the native home; see
    #: =internal/paseo/provider.go= in agent-quotas, which this mirrors. A
    #: record whose provider no longer resolves (a deleted handoff alias)
    #: falls back to the native agent Paseo stored on it. Every id field that
    #: could name the native session is offered, Codex's thread id first.
    ##
    (( $# )) || return 0
    ensure-cmd jq @RET
    local home cfg provs='{}'
    home="$(h-paseo-home)" @RET
    cfg="${home}/config.json"
    if test -r "${cfg}" ; then
        provs="$(command jq --compact-output '.agents.providers // {}' "${cfg}")" || provs='{}'
    fi

    command jq --raw-output --argjson provs "${provs}" '
        def walk_to($n; $env; $d):
          if $d > 16 or ($n | type) != "string" then null
          elif $n == "claude" or $n == "codex" then
            {agent: $n, env: ((($provs[$n].env // {}) | objects) + $env)}
          elif ($provs[$n] | type) == "object" then
            walk_to($provs[$n].extends; ((($provs[$n].env // {}) | objects) + $env); $d + 1)
          else null end;
        (walk_to(.provider; {}; 0)) as $n
        | ($n.agent // (.persistence.metadata.provider // "" | select(. == "claude" or . == "codex")) // "-") as $agent
        | (if $agent == "claude" then $n.env.CLAUDE_CONFIG_DIR
           elif $agent == "codex" then $n.env.CODEX_HOME else null end // "") as $native_home
        | ([.persistence.metadata.threadId?, .persistence.sessionId?,
            .persistence.nativeHandle?, .runtimeInfo.sessionId?]
           | map(select(type == "string" and test("^[0-9A-Fa-f][0-9A-Fa-f-]{7,}$")) | ascii_downcase)
           | reduce .[] as $i ([]; if index([$i]) then . else . + [$i] end)) as $ids
        | [ .id, $agent, $native_home, ($ids | join(",")), (.cwd // "-"), (.provider // "-"),
            (.lastStatus // "-"), (.archivedAt // "-"), (.title // "-"),
            ((.labels.keep // "") | tostring | ascii_downcase == "true"),
            (.labels["paseo.parent-agent-id"] // "-") ]
        | map(tostring | gsub("[\t\n]"; " ")) | join("\t")' "$@"
}

function h-paseo-native-files {
    : "usage: h-paseo-native-files <native agent> <native home> <ids>
The native files of one session, the main transcript first: for Claude the
transcript and its directory of subagent transcripts, for Codex every rollout
of the thread in sessions/ and archived_sessions/. Prints nothing when none is
on disk."
    #: The provider's own home is searched first; only when it holds nothing
    #: are the other homes this host knows tried, since a provider that sets
    #: no home ran with whatever the daemon had.
    ##
    setopt localoptions bareglobqual
    local agent="${1}" native_home="${2/#\~/${HOME}}" ids="${3}"

    local -a homes hits
    local h id
    case "${agent}" in
        claude) homes=( "${native_home:-${HOME}/.claude}/projects" ) ;;
        codex) homes=( "${native_home:-${HOME}/.codex}/sessions" ) ;;
        *) return 1 ;;
    esac
    local round
    for round in own other ; do
        if [[ "${round}" == other ]] ; then
            homes=( ${(f)"$(h-agent-session-call "${agent}" roots 2>/dev/null)"} )
        fi
        for h in "${homes[@]}" ; do
            test -n "${h}" || continue
            for id in ${(s:,:)ids} ; do
                case "${agent}" in
                    claude)
                        hits+=( "${h}"/*/"${id}".jsonl(N.) "${h}"/*/"${id}"(N/) ) ;;
                    codex)
                        hits+=( "${h}"/*/*/*/rollout-*-"${id}".jsonl(N.)
                                "${h:h}"/archived_sessions/rollout-*-"${id}".jsonl(N.) ) ;;
                esac
            done
        done
        (( ${#hits} )) && break
    done
    (( ${#hits} )) || return 0
    print -rl -- "${(@u)hits}"
}

function paseo-transcript-path {
    : "usage: paseo-transcript-path [ID]
The native transcript of a Paseo agent (id or unique prefix; default
\$PASEO_AGENT_ID). Fails, saying why, for an unsupported provider or a
transcript that is not on disk."
    local id="${1:-${PASEO_AGENT_ID:-}}"
    if test -z "${id}" ; then
        ecerr "$0: no agent id given, and PASEO_AGENT_ID is unset"
        return 64
    fi
    local record row
    record="$(h-paseo-agent-record "${id}")" @RET
    row="$(h-paseo-native-rows "${record}")" @RET
    local -a f
    f=( "${(@ps:\t:)row}" )
    if [[ "${f[2]}" == '-' ]] ; then
        ecerr "$0: agent ${f[1]} runs on provider ${f[6]}, which is not Claude Code or Codex; its transcript cannot be found"
        return 1
    fi
    if test -z "${f[4]}" ; then
        ecerr "$0: agent ${f[1]} has no native session id yet"
        return 1
    fi
    local p1
    for p1 in ${(f)"$(h-paseo-native-files "${f[2]}" "${f[3]}" "${f[4]}")"} ; do
        if [[ "${p1}" == *.jsonl ]] ; then
            ec "${p1}"
            return 0
        fi
    done
    ecerr "$0: no ${f[2]} transcript on disk for agent ${f[1]} (session ${f[4]})"
    return 1
}

function h-paseo-transcript-error {
    local band="${1}" message="${2}"
    ecerr "Paseo transcript: ${message}"
    if [[ "${agent_view_progress:-auto}" == hs || ( "${agent_view_progress:-auto}" == auto && ! -t 2 ) ]] ; then
        h-agent-view-banner "${band}" crit 8 0.35 "**Paseo transcript**: ${message}"
    fi
}

function h-paseo-transcript-local-host {
    local host="${1}" server_id="${2}" local_id="${PASEO_SERVER_ID:-}"
    local id_file="${paseo_home:-${PASEO_HOME:-${HOME}/.paseo}}/server-id"
    if [[ -z "${local_id}" && -r "${id_file}" ]] ; then
        local_id="$(<"${id_file}")"
    fi
    if [[ -n "${server_id}" ]] ; then
        [[ "${server_id}" == "${local_id}" ]]
        return
    fi
    [[ "${host}" == "${local_id}" ]] && return 0

    local name
    if [[ -x /usr/sbin/scutil ]] ; then
        name="$(/usr/sbin/scutil --get ComputerName 2>/dev/null)" || name=''
    fi
    [[ -n "${name}" ]] || name="$(command hostname)" @RET
    [[ "${(L)host}" == "${(L)name}" ]]
}

function h-paseo-transcript-ssh-alias {
    setopt localoptions extendedglob
    local host="${1}" server_id="${2}" config="${HOME}/.config/ypaseo/transcript-hosts"
    local line label alias mapped=''
    if [[ -r "${config}" ]] ; then
        while IFS= read -r line || [[ -n "${line}" ]] ; do
            line="${line%%\#*}"
            line="${${line##[[:blank:]]#}%%[[:blank:]]#}"
            [[ "${line}" == *[[:blank:]]* ]] || continue
            alias="${line##*[[:blank:]]}"
            label="${line%${alias}}"
            label="${label%%[[:blank:]]#}"
            if [[ -n "${server_id}" && "${label}" == "${server_id}" ]] ; then
                mapped="${alias}"
                break
            elif [[ -z "${mapped}" && "${(L)label}" == "${(L)host}" ]] ; then
                mapped="${alias}"
            fi
        done < "${config}"
    fi
    alias="${mapped:-${(L)host}}"
    if [[ ! "${alias}" =~ '^[A-Za-z0-9][A-Za-z0-9._-]{0,62}$' ]] ; then
        ecerr "Configure a valid SSH alias for ${host} in ~/.config/ypaseo/transcript-hosts."
        return 64
    fi
    print -r -- "${alias}"
}

function h-paseo-transcript-stream {
    local transcript="${1}" format="${2:e}"
    case "${format}" in
        org) h-agent-session-render-document org org-pandoc "${transcript}" ;;
        md) h-agent-session-render-document md md "${transcript}" ;;
        jsonl) command cat -- "${transcript}" ;;
    esac
}

function paseo-transcript-render {
    : "usage: paseo-transcript-render ID org|md|jsonl
SSH endpoint. Streams a transcript on stdout and progress on stderr while
stdin remains open."
    local id="${(L)1}" format="${2}"
    if [[ ! "${id}" =~ '^[0-9a-f][0-9a-f-]{0,35}$' || "${format}" != (org|md|jsonl) ]] ; then
        ecerr "Invalid transcript id or format."
        return 64
    fi
    local -x PASEO_HOST=''
    local agent_view_key='' agent_view_label='' agent_view_initial_progress=''
    local transcript tmp
    transcript="$(paseo-transcript-path "${id}")" @RET
    tmp="$(gmktemp --directory)" @TRET
    {
        #: The native session key also covers short IDs and multiple records
        #: referring to the same provider session on this host.
        agent_view_progress=wire h-agent-view-transcript-run paseo-transcript-render '' \
            h-paseo-transcript-stream "${transcript}" "${tmp}/transcript.${format}"
    } always {
        h-agent-view-rm-tmp "${tmp}"
    }
}

function h-paseo-transcript-copy-status {
    local progress="${1}" error_file="${2}" unsupported="${3:-}" metadata="${4:-}" line payload
    while IFS= read -r line || [[ -n "${line}" ]] ; do
        if [[ "${line}" == 'YPASEO-PROGRESS '* ]] ; then
            payload="${line#YPASEO-PROGRESS }"
            if [[ "${payload}" =~ '^[0-9]{1,3} (download|read|render|pandoc|done) [0-9]{1,16} [0-9]{1,16}$' ]] ; then
                print -r -- "${payload}" > "${progress}.next"
                command mv -f -- "${progress}.next" "${progress}"
            fi
        elif [[ -n "${metadata}" && "${line}" == 'YPASEO-TRANSCRIPT-FILE '* ]] ; then
            print -r -- "${line#YPASEO-TRANSCRIPT-FILE }" > "${metadata}"
        elif [[ -n "${unsupported}" && ( "${line}" == 'YPASEO-TRANSCRIPT-UNSUPPORTED' ||
            "${line}" == "error: unknown command 'transcript'" ) ]] ; then
            : > "${unsupported}"
        elif [[ -n "${line}" && ! -s "${error_file}" ]] ; then
            print -r -- "${line}" > "${error_file}"
        fi
    done
}

function h-paseo-transcript-over-ssh {
    local host="${1}" out="${2}" format="${2:e}"
    local input="${out:h}/ssh.in" status_pipe="${out:h}/ssh.err"
    local fd reader result=0
    command mkfifo -- "${input}" "${status_pipe}" @RET
    #: Keep SSH stdin open without a writer process. The remote endpoint uses
    #: EOF to stop its worker even if the local SSH process was SIGKILLed.
    exec {fd}<> "${input}" @RET
    h-paseo-transcript-copy-status "${agent_session_progress_file}" "${out:h}/error" < "${status_pipe}" &
    reader=$!
    #: Only the validated hex id/prefix and format enter the remote command.
    command ssh -T -S none -o BatchMode=yes -o ConnectTimeout=10 -o StdinNull=no \
        -o ServerAliveInterval=5 -o ServerAliveCountMax=2 -- "${host}" \
        'exec zsh -c '\''if (( $+functions[paseo-transcript-render] )); then paseo-transcript-render "$1" "$2"; else print -ru2 -- "Remote transcript renderer is missing. Update ~/scripts on that host."; exit 127; fi'\'' ypaseo-transcript '"${paseo_transcript_id} ${format}" \
        <&${fd} > "${out}" 2> "${status_pipe}" || result=$?
    exec {fd}>&-
    wait "${reader}"
    command rm -f -- "${input}" "${status_pipe}"
    #: A remote native-session takeover may come through another alias or
    #: prefix. It has its own band, unlike a local takeover of this request.
    (( result == 3 )) && return 75
    if (( result == 0 )) && [[ "${format}" != jsonl ]] ; then
        if [[ ! -r "${agent_session_progress_file}" || "$(<"${agent_session_progress_file}")" != '100 done '* ]] ; then
            return 1
        fi
    fi
    return "${result}"
}

function h-paseo-transcript-from-host {
    local host="${1}" out="${2}" native="${2:r}.jsonl"
    local status_pipe="${out:h}/daemon.err" unsupported="${out:h}/unsupported"
    local error_file="${out:h}/error" metadata="${out:h}/metadata"
    local destination="${paseo_transcript_server_id:-${host}}"
    local home="${paseo_home:-${PASEO_HOME:-${HOME}/.paseo}}"
    local reader result=0 ssh_alias
    command mkfifo -- "${status_pipe}" @RET
    h-paseo-transcript-copy-status "${agent_session_progress_file}" "${error_file}" "${unsupported}" "${metadata}" < "${status_pipe}" &
    reader=$!
    command paseo agent transcript "${paseo_transcript_id}" --home "${home}" \
        --destination "${destination}" --progress --output "${native}" 2> "${status_pipe}" || result=$?
    wait "${reader}"
    command rm -f -- "${status_pipe}"
    if (( result == 0 )) ; then
        [[ "${out:e}" == jsonl ]] && return 0
        local info filename
        if ! info="$(command jq -er '
            select(type == "array" and length == 2)
            | select(.[0] == "claude" or .[0] == "codex")
            | select(.[1] | test("^[A-Za-z0-9][A-Za-z0-9._ -]*\\.jsonl$"))
            | join(" ")
        ' "${metadata}" 2>/dev/null)" ; then
            print -r -- 'The daemon did not identify the downloaded transcript file.' > "${error_file}"
            return 1
        fi
        filename="${info#* }"
        local agent_session_provider_hint="${info%% *}"
        local agent_session_provider_hint_path="${out:h}/native/${filename}"
        command mkdir -- "${agent_session_provider_hint_path:h}" @RET
        command mv -- "${native}" "${agent_session_provider_hint_path}" @RET
        print -r -- '0 render 0 0' > "${agent_session_progress_file}"
        h-paseo-transcript-stream "${agent_session_provider_hint_path}" "${out:e}" > "${out}" 2> "${error_file}" || return $?
        command rm -f -- "${agent_session_provider_hint_path}" "${metadata}"
        command rmdir -- "${agent_session_provider_hint_path:h}"
        return 0
    fi

    [[ -f "${unsupported}" ]] || return "${result}"
    print -ru2 -- "Paseo transcript: daemon download unavailable for ${host}. Falling back to SSH."
    command rm -f -- "${error_file}" "${agent_session_progress_file}"
    if ! ssh_alias="$(h-paseo-transcript-ssh-alias "${host}" "${paseo_transcript_server_id:-}" 2>&1)" ; then
        print -r -- "${ssh_alias}" > "${error_file}"
        return 64
    fi
    result=0
    h-paseo-transcript-over-ssh "${ssh_alias}" "${out}" || result=$?
    if (( result == 255 )) ; then
        print -r -- "Could not reach ${host} over SSH (${ssh_alias})." > "${error_file}"
    fi
    return "${result}"
}

function h-paseo-view-remote-transcript {
    local host="${1}" ext="${2}" paseo_transcript_id="${3}"
    local key tmp band out result=0 reason
    #: Desktop requests carry the server ID. Use it for the claim so a host
    #: label change still cancels the download of the same agent.
    local destination="${paseo_transcript_server_id:-${(L)host}}"
    key="$(printf '%s\n%s\n' "${destination}" "${paseo_transcript_id}" | command shasum -a 256)" @RET
    key="paseo-remote-${key%% *}"
    band="$(h-agent-view-name-of "${key}")" @RET
    tmp="$(gmktemp --directory)" @TRET
    out="${tmp}/${paseo_transcript_id}.${ext}"
    local agent_view_key="${key}" agent_view_label="Paseo transcript from ${host}"
    local agent_view_initial_progress="Connecting to ${host}…"
    {
        h-agent-view-transcript-run "${band}" 'run again to restart' \
            h-paseo-transcript-from-host "${host}" "${out}" || result=$?
        case "${result}" in
            0)
                if emc-open "${out}" ; then
                    h-agent-view-dismiss "${band}"
                    return 0
                fi
                result=1
                reason="Emacs did not open the transcript from ${host}."
                ;;
            3) return 0 ;;
            75) h-agent-view-dismiss "${band}"; return 0 ;;
            *)
                [[ -s "${tmp}/error" ]] && reason="$(<"${tmp}/error")"
                if [[ "${reason}" == 'flag provided but not defined: -progress-file' ]] ; then
                    reason='Remote transcript renderer is too old. Update ~/scripts on that host.'
                fi
                reason="${host}: ${reason:-Remote transcript conversion failed.}"
                ;;
        esac
        h-paseo-transcript-error "${band}" "${reason}"
        return "${result}"
    } always {
        if (( result != 0 )) ; then
            h-agent-view-rm-tmp "${tmp}"
        fi
    }
}

function h-paseo-view-transcript {
    #: Usage: h-paseo-view-transcript <converter> <ext> [--host HOST] [ID]
    local converter="${1}" ext="${2}" host='' id transcript
    local server_id="${paseo_transcript_server_id:-}"
    shift 2
    if [[ "${1:-}" == --host ]] ; then
        host="${2:-}"
        if [[ ! "${host}" =~ '^[A-Za-z0-9][A-Za-z0-9._ -]{0,62}$' ]] ; then
            h-paseo-transcript-error paseo-view-transcript 'Invalid host label or server id.'
            return 64
        fi
        shift 2
    fi
    id="${1:-${PASEO_AGENT_ID:-}}"
    if (( $# > 1 )) ; then
        h-paseo-transcript-error paseo-view-transcript 'Expected one Paseo agent id.'
        return 64
    fi
    local -x PASEO_HOST="${PASEO_HOST:-}"
    if [[ -n "${host}" ]] ; then
        if ! h-paseo-transcript-local-host "${host}" "${server_id}" ; then
            id="${(L)id}"
            if [[ ! "${id}" =~ '^[0-9a-f][0-9a-f-]{0,35}$' ]] ; then
                h-paseo-transcript-error paseo-view-transcript 'Expected a Paseo agent id or unique prefix.'
                return 64
            fi
            h-paseo-view-remote-transcript "${host}" "${ext}" "${id}"
            return
        fi
        PASEO_HOST=''
    fi
    if ! transcript="$(paseo-transcript-path "${id}" 2>&1)" ; then
        h-paseo-transcript-error paseo-view-transcript "${transcript#*: }"
        return 1
    fi
    if [[ "${ext}" == jsonl ]] ; then
        emc-open "${transcript}"
    else
        h-agent-view-session-transcript "${converter}" "${ext}" "${transcript}"
    fi
}

function paseo-view-transcript {
    : "usage: paseo-view-transcript [--host HOST] [ID]
Converts a Paseo agent's native transcript to org and opens it in emacs, with
progress; running it again for the same session restarts it. ID defaults to
\$PASEO_AGENT_ID."
    h-paseo-view-transcript h-agent-session-to-org org "$@"
}

function paseo-view-transcript-md {
    : "usage: paseo-view-transcript-md [--host HOST] [ID]
Like paseo-view-transcript, as markdown."
    h-paseo-view-transcript h-agent-session-to-md md "$@"
}

function paseo-view-transcript-raw {
    : "usage: paseo-view-transcript-raw [--host HOST] [ID]
Opens a Paseo agent's native transcript itself in emacs."
    h-paseo-view-transcript '' jsonl "$@"
}

function paseo-view-transcript-fz {
    : "usage: paseo-view-transcript-fz [query]
Picks a Paseo agent (title, status, provider, cwd) and opens its transcript as
org. alt-m opens markdown, alt-r the raw transcript."
    ensure-cmd jq @RET
    local rows sel
    rows="$(paseo-ls --json | command jq --raw-output '
        sort_by(.lastActivityAt // "") | reverse | .[]
        | [ .id, .title, .status, .provider, .cwd ] | map(tostring | gsub("[\t\n]"; " ")) | join("\t")')" @RET
    test -n "${rows}" || { ecerr "$0: no Paseo agents"; return 1; }
    sel="$(ec "${rows}" | fzf-gateway --delimiter=$'\t' --with-nth=2.. --query "${*}" \
        --expect=alt-m,alt-r --header='TITLE | STATUS | PROVIDER | CWD; enter org, alt-m markdown, alt-r raw')" @RET
    local key="${${(@f)sel}[1]}" row="${${(@f)sel}[2]}"
    test -n "${row}" || return 0
    case "${key}" in
        alt-m) paseo-view-transcript-md "${row%%$'\t'*}" ;;
        alt-r) paseo-view-transcript-raw "${row%%$'\t'*}" ;;
        *) paseo-view-transcript "${row%%$'\t'*}" ;;
    esac
}

##
#: Deleting archived sessions for good. See =docs/paseo-transcript-delete.md=.
##
function h-paseo-transcript-delete-catalog {
    : "usage: h-paseo-transcript-delete-catalog <dir>
Writes <dir>/rows (the picker's rows, oldest archive first) and <dir>/files
(id TAB native path) for the archived agents that may be deleted."
    #: Eligible: archived, not internal, not pinned (keep), a Claude Code or
    #: Codex provider, no other record (archived or not) naming the same native
    #: session, and no other record naming it as its parent, since deleting a
    #: parent must not cascade.
    ##
    setopt localoptions bareglobqual
    local out="${1}"
    assert-args out @RET
    local home
    home="$(h-paseo-home)" @RET
    local -a records
    records=( "${home}"/agents/*.json(N.) "${home}"/agents/*/*.json(N.) )
    : > "${out}/rows" ; : > "${out}/files"
    (( ${#records} )) || return 0

    local all
    all="$(h-paseo-native-rows "${records[@]}")" @RET
    local -A owners parents
    local row id
    local -a f
    for row in ${(f)all} ; do
        f=( "${(@ps:\t:)row}" )
        for id in ${(s:,:)f[4]} ; do
            owners[${id}]="${owners[${id}]:+${owners[${id}]} }${f[1]}"
        done
        [[ "${f[11]}" != '-' ]] && parents[${f[11]}]=1
    done

    local shared p1 size total
    local -a lines files
    for row in ${(f)all} ; do
        f=( "${(@ps:\t:)row}" )
        [[ "${f[8]}" != '-' && "${f[2]}" != '-' && "${f[10]}" != true ]] || continue
        (( ${+parents[${f[1]}]} )) && continue
        shared=n
        for id in ${(s:,:)f[4]} ; do
            [[ "${owners[${id}]}" == "${f[1]}" ]] || shared=y
        done
        [[ "${shared}" == n ]] || continue

        files=( ${(f)"$(h-paseo-native-files "${f[2]}" "${f[3]}" "${f[4]}")"} )
        total=0
        for p1 in "${files[@]}" ; do
            print -r -- "${f[1]}"$'\t'"${p1}" >> "${out}/files"
            size="$(command du -sk -- "${p1}" 2>/dev/null)" && total=$(( total + ${size%%[[:space:]]*} ))
        done
        lines+=( "${f[8]}"$'\t'"${f[1]}"$'\t'"${f[1][1,8]}"$'\t'"${f[8]}"$'\t'"${f[6]}"$'\t'"$(( total / 1024.0 ))"$'\t'"${f[9]}"$'\t'"${f[5]}" )
    done
    (( ${#lines} )) || return 0
    #: Sorted on the ISO timestamp in front, which is then dropped.
    print -rl -- "${(@o)lines}" | command cut -f2- \
        | command awk -F'\t' -v OFS='\t' '{ $5 = sprintf("%.1fM", $5) ; print }' > "${out}/rows"
}

function paseo-transcript-delete-fz {
    : "usage: paseo-transcript-delete-fz [--dry-run] [--yes] [--filter TEXT] [QUERY]
Pick archived Paseo sessions and permanently delete their Paseo and native transcripts."
    emulate -L zsh
    setopt pipefail
    local home dry=n yes=n filter='' filter_p=n query='' selected row tmp
    local -a ids=() fz_args=()
    while (( $# )) ; do
        case "${1}" in
            -n|--dry-run) dry=y ;;
            -y|--yes) yes=y ;;
            --filter)
                (( $# >= 2 )) || { ecerr "$0: --filter needs text"; return 64; }
                filter="${2}" filter_p=y ; shift ;;
            -h|--help)
                print -r -- 'usage: paseo-transcript-delete-fz [-n|--dry-run] [-y|--yes] [--filter TEXT] [QUERY]
Lists archived sessions on this host, oldest archive first. Tab selects multiple
rows. Shows the exact native files, then confirms permanent deletion (default No).
--dry-run previews without deleting; --yes skips confirmation. --filter performs
non-interactive fzf selection and still requires --yes for deletion without a tty.
Codex: rollouts in sessions and archived_sessions. Claude: the main transcript
and its native subagent transcripts. Configured provider aliases are supported.
Paseo records and timelines are removed through paseo agent delete. Other Paseo
sessions are never cascaded. Shared native sessions, pins, unsupported harnesses
and remote PASEO_HOST targets are refused. See docs/paseo-transcript-delete.md.'
                return 0 ;;
            --) shift ; query="${*}" ; break ;;
            -*) ecerr "$0: unknown option: ${1}"; return 64 ;;
            *) query+="${query:+ }${1}" ;;
        esac
        shift
    done
    ensure-cmd fzf jq @RET
    home="$(h-paseo-home)" @RET
    tmp="$(command mktemp -d "${TMPDIR:-/tmp}/paseo-transcript-delete.XXXXXXXX")" @RET
    {
        h-paseo-transcript-delete-catalog "${tmp}" @RET
        if [[ ! -s "${tmp}/rows" ]] ; then
            print -r -- 'No eligible archived sessions.'
            return 0
        fi
        local preview="command awk -F'\t' -v id={1} '\$1 == id { print \$2 }' $(gquote "${tmp}/files")"
        fz_args=(--multi --no-sort --delimiter=$'\t' --with-nth=2.. \
            --query "${query}" --header='ARCHIVED: ID | ARCHIVED AT | PROVIDER | NATIVE SIZE | TITLE | CWD; Tab selects; Enter reviews deletion' \
            --preview "${preview}" --preview-window=down,60%,wrap)
        [[ "${filter_p}" == y ]] && fz_args+=(--filter "${filter}")
        selected="$(fzf-gateway "${fz_args[@]}" < "${tmp}/rows")" || return $?
        [[ -n "${selected}" ]] || return 0
        for row in "${(@f)selected}" ; do
            ids+=("${row%%$'\t'*}")
        done

        local id p1
        local -a doomed
        for id in "${ids[@]}" ; do
            print -r -- "${id}  $(command awk -F'\t' -v id="${id}" '$1 == id { print $5 " " $6 }' "${tmp}/rows")"
            doomed=( ${(f)"$(command awk -F'\t' -v id="${id}" '$1 == id { print $2 }' "${tmp}/files")"} )
            if (( ${#doomed} == 0 )) ; then
                print -r -- '    (no native files on disk)'
            fi
            for p1 in "${doomed[@]}" ; do
                print -r -- "    ${p1}"
            done
        done
        [[ "${dry}" == y ]] && return 0
        ensure-cmd paseo @RET
        if [[ "${yes}" != y ]] ; then
            if ! { : < /dev/tty } 2>/dev/null ; then
                ecerr "$0: confirmation needs a terminal; use --yes (or --dry-run)"
                return 64
            fi
            ask "Permanently delete these ${#ids} archived sessions and their native transcripts?" N || return 1
        fi

        local failed=0
        for id in "${ids[@]}" ; do
            #: The record goes first: a record left pointing at deleted files is
            #: worse than files left behind by a record that is gone.
            if ! command paseo agent delete "${id}" --home "${home}" > /dev/null ; then
                ecerr "$0: paseo agent delete failed for ${id}; its native files are kept"
                failed=$(( failed + 1 ))
                continue
            fi
            doomed=( ${(f)"$(command awk -F'\t' -v id="${id}" '$1 == id { print $2 }' "${tmp}/files")"} )
            for p1 in "${doomed[@]}" ; do
                test -n "${p1}" || continue
                command rm -rf -- "${p1}" || failed=$(( failed + 1 ))
            done
            print -r -- "deleted ${id}"
        done
        (( failed == 0 ))
    } always {
        command rm -rf -- "${tmp}"
    }
}
