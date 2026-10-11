##
#: Native session handoff to Paseo. See =docs/paseo.md=.
##
function paseo-install {
    : "usage: paseo-install [version]"
    local package='@getpaseo/cli'
    (( $# <= 1 )) || { ecerr "$0: expected at most one version"; return 64; }
    [[ -n "${1:-}" ]] && package+="@${1}"
    local -a npm_install_pnpm_opts=(--allow-build=esbuild,msgpackr-extract,node-pty)
    npm-install "${package}" @RET
    h-npm-install-report paseo
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
